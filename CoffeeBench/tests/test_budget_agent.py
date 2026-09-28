import asyncio
import json
from types import SimpleNamespace

import pytest

from coffeebench import main
from coffeebench.agent import Agent
from coffeebench.config import RunConfig, validate_agent_execution
from coffeebench.models.passive_model import PassiveModel
from coffeebench.models.types import ModelResponse, ToolCall


class FakeModel(PassiveModel):
    def __init__(self):
        super().__init__("gpt-test")
        self.requests = []
        self.plan = {"memory": "remember", "actions": []}

    def query(self, messages, tools=None):
        self.n_calls += 1
        self.requests.append(messages)
        if isinstance(self.plan, Exception):
            raise self.plan
        plan = (
            self.plan(json.loads(messages[-1]["content"]))
            if callable(self.plan)
            else self.plan
        )
        return ModelResponse(tool_calls=[ToolCall("p", "submit_plan", plan)])


def make_env(tmp_path, monkeypatch, mode="budget", days=2, settings=""):
    monkeypatch.chdir(tmp_path)
    monkeypatch.setattr(main, "get_model", lambda _: FakeModel())
    config = tmp_path / "test.toml"
    config.write_text(f'''[experiment]
name = "test"
[run]
max_days = {days}
[models]
default = "gpt-test"
[agent_execution]
mode = "{mode}"
{settings}
''')
    args = SimpleNamespace(
        config=str(config),
        max_days=None,
        seed=0,
        model=None,
        models=None,
        main_agent=None,
    )
    env, _, _ = main.build_run(args)
    env.verbose = False
    return env


def test_default_and_invalid_settings(tmp_path):
    path = tmp_path / "empty.toml"
    path.write_text('[experiment]\nname="legacy"')
    assert RunConfig.from_toml(path).agent_execution == {}
    for settings in (
        {"mode": "oops"},
        {"decisions_per_day": 0},
        {"max_actions": True},
        {"history_limit": -1},
        {"other": 2},
    ):
        with pytest.raises(ValueError):
            validate_agent_execution(settings)


def test_budget_run_has_two_calls_per_day_and_fresh_inputs(tmp_path, monkeypatch):
    env = make_env(tmp_path, monkeypatch)
    result = asyncio.run(env.run())
    for agent in env.agents.values():
        assert agent.decision_calls == 4
        assert agent.decision_errors == 0
        assert agent.model.n_calls == 4
        snapshots = [json.loads(r[-1]["content"]) for r in agent.model.requests]
        assert [s["at"] for s in snapshots] == [540, 840, 1980, 2280]
        assert all(len(r) == 2 for r in agent.model.requests)
        assert snapshots[1]["memory"] == "remember"
    assert result["agent_execution"]["mode"] == "budget"


def test_react_uses_original_class(tmp_path, monkeypatch):
    env = make_env(tmp_path, monkeypatch, mode="react")
    assert all(type(a) is Agent for a in env.agents.values())
    env.event_logger.close()


def test_cli_override_restores_react(tmp_path, monkeypatch):
    env = make_env(tmp_path, monkeypatch)
    env.event_logger.close()
    args = SimpleNamespace(
        config=str(tmp_path / "test.toml"),
        max_days=None,
        seed=0,
        model=None,
        models=None,
        main_agent=None,
        agent_mode="react",
    )
    env, _, _ = main.build_run(args)
    assert all(type(a) is Agent for a in env.agents.values())
    assert env.agent_execution["mode"] == "react"
    env.event_logger.close()


def test_queued_actions_use_one_call_and_normal_time(tmp_path, monkeypatch):
    env = make_env(tmp_path, monkeypatch, days=1)
    a = env.agents["retailer_A"]
    a.model.plan["actions"] = [
        {
            "name": "set_retail_price",
            "arguments_json": json.dumps(
                {"item_id": "roasted_coffee_kg", "price_per_unit": p}
            ),
        }
        for p in (20, 21)
    ]
    asyncio.run(env.run())
    assert a.decision_calls == 2
    events = [
        json.loads(line)
        for line in (tmp_path / "trajectories/test/seed_0/run.events.jsonl")
        .read_text()
        .splitlines()
    ]
    steps = [
        e
        for e in events
        if e["type"] == "agent_step"
        and e["agent_id"] == "retailer_A"
        and e["action"] == "set_retail_price"
    ]
    assert [e["at"] for e in steps] == [540, 570, 840, 870]
    assert all(e["observation"]["status"] == "success" for e in steps)


@pytest.mark.parametrize(
    "plan",
    [
        RuntimeError("offline"),
        {"memory": "", "actions": [{"name": "pay_invoice", "arguments_json": "bad"}]},
        {"memory": "", "actions": [{"name": "unknown", "arguments_json": "{}"}]},
        {
            "memory": "",
            "actions": [{"name": "wait_for_next_day", "arguments_json": "{}"}] * 5,
        },
    ],
)
def test_failures_consume_budget_without_retry(tmp_path, monkeypatch, plan):
    env = make_env(tmp_path, monkeypatch, days=1)
    a = env.agents["roaster_A"]
    a.model.plan = plan
    asyncio.run(env.run())
    assert a.model.n_calls == 2
    assert a.decision_errors == 2


def test_reactive_wake_does_not_bypass_budget_and_snapshot_is_private(
    tmp_path, monkeypatch
):
    env = make_env(tmp_path, monkeypatch)
    a = env.agents["roaster_A"]
    env.time_manager.virtual_min = 540
    a.init()
    a.step_apply(a.step_query())
    env.marketplace.post_message("retailer_A", "retailer_B", "secret", "private text")
    env.marketplace.post_message("retailer_A", "roaster_A", "hello", "visible text")
    assert "private text" not in json.dumps(a._observation())
    assert "visible text" in json.dumps(a._observation())
    env._wake_agent_externally("roaster_A", "new message")
    a.step_query()
    assert a.model.n_calls == 1
    assert a.next_ready_at(541) == 840
    env.event_logger.close()


def test_tool_failure_cancels_remainder(tmp_path, monkeypatch):
    env = make_env(tmp_path, monkeypatch)
    a = env.agents["roaster_A"]
    env.time_manager.virtual_min = 540
    a.init()
    a.model.plan["actions"] = [
        {"name": "accept_offer", "arguments_json": '{"offer_id":"missing"}'},
        {"name": "wait_for_next_day", "arguments_json": "{}"},
    ]
    result = a.step_apply(a.step_query())
    assert result["observation"]["status"] == "error"
    assert not a.pending
    assert a.next_ready_at(570) == 840
    env.event_logger.close()


def test_application_retry_override_is_scoped():
    from coffeebench.models._retry import attempt_limit, call_with_retry

    count = 0

    def fail():
        nonlocal count
        count += 1
        raise RuntimeError("timeout")

    token = attempt_limit.set(1)
    try:
        with pytest.raises(RuntimeError):
            call_with_retry(fail, base_delay=0, max_delay=0)
        assert count == 1
    finally:
        attempt_limit.reset(token)
    with pytest.raises(RuntimeError):
        call_with_retry(fail, max_attempts=2, base_delay=0, max_delay=0)
    assert count == 3


def test_non_divisible_slots_and_day_boundary(tmp_path, monkeypatch):
    env = make_env(tmp_path, monkeypatch, settings="decisions_per_day=7")
    a = env.agents["roaster_A"]
    a.init()
    for day in range(2):
        for slot in range(7):
            now = day * 1440 + 540 + slot * 600 // 7
            env.time_manager.virtual_min = now
            a.step_apply(a.step_query())
            assert slot in a.used_slots
            assert a.next_ready_at(now) > now
    assert a.model.n_calls == 14
    env.event_logger.close()


@pytest.mark.parametrize("research_enabled", [False, True])
def test_budget_trade_delivers_and_settles_with_original_tools(
    tmp_path, monkeypatch, research_enabled
):
    env = make_env(tmp_path, monkeypatch, days=3)
    if research_enabled:
        from coffeebench.research import CircularResearch

        env.research = CircularResearch(
            env, {"enabled": True, "supply_stop_day": 1}, {}
        )
    monkeypatch.setattr("coffeebench.environment.DELIVERY_LOSS_PROB", 0)
    monkeypatch.setattr("coffeebench.environment.DELIVERY_DELAY_PROB", 0)

    def plan(name=None, **args):
        return {
            "memory": "",
            "actions": []
            if name is None
            else [{"name": name, "arguments_json": json.dumps(args)}],
        }

    def seller(s):
        if s["at"] == 540:
            return plan(
                "post_listing",
                item_id="roasted_coffee_kg",
                qty=2,
                asking_price=10,
                payment_terms_days=0,
            )
        for offer in s["offers"]["offers"]:
            if offer["status"] == "pending":
                return plan("accept_offer", offer_id=offer["id"])
        return plan()

    def buyer(s):
        for invoice in s["payables"]["rows"]:
            return plan("pay_invoice", invoice_id=invoice["id"])
        if not s["offers"]["offers"]:
            for listing in s["listings"]["listings"]:
                if listing["seller_id"] == "roaster_A":
                    return plan(
                        "make_offer",
                        listing_id=listing["id"],
                        offered_price=10,
                        qty=2,
                        payment_terms_days=0,
                    )
        return plan()

    env.agents["roaster_A"].model.plan = seller
    env.agents["retailer_A"].model.plan = buyer
    asyncio.run(env.run())
    assert len(env.marketplace.deals) == 1
    deal = env.marketplace.deals[0]
    invoices = env.business_apps["retailer_A"].accounts_payable
    assert len(invoices) == 1
    assert invoices[0].paid
    assert invoices[0].reference == deal.id
    assert any(
        e.entry_type == "sale_revenue" and e.reference == deal.id and e.amount == 20
        for e in env.truth_ledger["roaster_A"]
    )
    assert env.agents["roaster_A"].decision_errors == 0
    assert env.agents["retailer_A"].decision_errors == 0
