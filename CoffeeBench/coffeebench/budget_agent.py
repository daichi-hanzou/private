"""Bounded planning calls; ordinary tools still execute one at a time."""

import json
from collections import deque
from dataclasses import asdict

from jsonschema import validate

from coffeebench.agent import Agent
from coffeebench.models.types import ModelResponse, ToolCall, ToolSpec


class BudgetAgent(Agent):
    def __init__(
        self,
        *,
        business_app,
        decisions_per_day=2,
        max_actions=4,
        history_limit=10,
        memory_chars=1200,
        kpi=None,
        **kwargs,
    ):
        super().__init__(**kwargs)
        self.ba = business_app
        self.decisions_per_day = decisions_per_day
        self.max_actions = max_actions
        self.history_limit = history_limit
        self.memory_chars = memory_chars
        self.kpi = kpi or {"metric": "net_income"}
        self.pending = deque()
        self.recent_results = deque(maxlen=history_limit)
        self.memory = ""
        self.used_slots = set()
        self.plan_day = None
        self.decision_calls = 0
        self.decision_errors = 0
        self.last_plan = None
        self.action_specs = {
            t.name: t
            for t in self.tool_specs
            if not t.name.startswith("view_") and t.name != "read_message"
        }
        self.plan_spec = ToolSpec(
            "submit_plan",
            "Submit a bounded ordered plan.",
            {
                "type": "object",
                "additionalProperties": False,
                "properties": {
                    "memory": {"type": "string", "maxLength": memory_chars},
                    "actions": {
                        "type": "array",
                        "maxItems": max_actions,
                        "items": {
                            "type": "object",
                            "additionalProperties": False,
                            "properties": {
                                "name": {
                                    "type": "string",
                                    "enum": list(self.action_specs),
                                },
                                "arguments_json": {"type": "string"},
                            },
                            "required": ["name", "arguments_json"],
                        },
                    },
                },
                "required": ["memory", "actions"],
            },
        )
        self.system_prompt += (
            "\nBUDGET MODE overrides instructions to query view tools. "
            "You receive a consolidated private observation. Call submit_plan once "
            "with memory and an ordered list of actions (possibly empty). "
            "Each arguments_json is a JSON object matching the named action schema. "
            "Actions execute separately with normal time costs and validation. "
            "Use only IDs already observed; new IDs cannot be referenced in this plan. "
            "A failed action cancels the remaining plan. wait_for_next_day ends it. "
            "Include send_message as an action for communication. "
            "Only the bounded memory and recent results persist in your next input. "
            f"You have at most {decisions_per_day} planning calls per day.\n"
            + json.dumps(
                {
                    k: {"description": v.description, "parameters": v.input_schema}
                    for k, v in self.action_specs.items()
                }
            )
        )

    def next_ready_at(self, now):
        from coffeebench.environment import BUSINESS_HOURS_START, BUSINESS_HOURS_END

        day, minute = divmod(now, 1440)
        if self.plan_day != day:
            self.pending.clear()
            self.used_slots.clear()
            self.plan_day = day
        if self.pending:
            return now
        width = BUSINESS_HOURS_END - BUSINESS_HOURS_START
        for slot in range(self.decisions_per_day):
            start = BUSINESS_HOURS_START + slot * width // self.decisions_per_day
            end = BUSINESS_HOURS_START + (slot + 1) * width // self.decisions_per_day
            if slot not in self.used_slots and minute < end:
                return day * 1440 + max(start, minute)
        return (day + 1) * 1440 + BUSINESS_HOURS_START

    def _observation(self):
        ba = self.ba
        env = ba.marketplace._env
        messages = ba.marketplace.messages_visible_to(ba.agent_id)
        own_deals = [
            d for d in ba.marketplace.deals if ba.agent_id in (d.seller_id, d.buyer_id)
        ]
        revenue = sum(
            e.amount * (-1 if e.entry_type == "sale_reversal" else 1)
            for e in env.truth_ledger.get(ba.agent_id, [])
            if e.entry_type in ("sale_revenue", "sale_reversal")
        )
        target = self.kpi.get("target_usd")
        return {
            "at": ba.time_manager.get_virtual_min(),
            "remaining_days": env.max_days - ba._today(),
            "observation": env._format_observation(ba.agent_id, ba._today(), None),
            "cash": ba.cash,
            "inventory": dict(ba.inventory),
            "inventory_total_cost": dict(ba.inventory_total_cost),
            "trial_balance": ba.view_trial_balance(),
            "recent_consumer_sales": ba.view_consumer_sales(
                since_day=max(0, ba._today() - 2)
            ),
            "kpi": self.kpi,
            "revenue_net": revenue,
            "target_shortfall": max(0, target - revenue)
            if target is not None
            else None,
            "listings": ba.view_listings(only_others=False),
            "offers": ba.view_offers(),
            "payables": ba.view_payables(),
            "receivables": ba.view_receivables(),
            "recent_deals": [asdict(d) for d in own_deals[-self.history_limit :]],
            "messages": [asdict(m) for m in messages[-self.history_limit :]],
            "memory": self.memory,
            "recent_results": list(self.recent_results),
        }

    def step_query(self):
        now = self.ba.time_manager.get_virtual_min()
        if self.next_ready_at(now) > now:
            return ModelResponse(content="Waiting for next decision slot.")
        if self.pending:
            return ModelResponse(tool_calls=[self.pending.popleft()])
        from coffeebench.environment import BUSINESS_HOURS_START, BUSINESS_HOURS_END

        minute = now % 1440
        slot = max(
            i
            for i in range(self.decisions_per_day)
            if BUSINESS_HOURS_START
            + i * (BUSINESS_HOURS_END - BUSINESS_HOURS_START) // self.decisions_per_day
            <= minute
        )
        self.used_slots.add(slot)
        self.decision_calls += 1  # Failures consume the slot too.
        snapshot = self._observation()
        # Keep the full transcript on disk, but never resend it to the model.
        request = [{"role": "user", "content": json.dumps(snapshot, default=str)}]
        if not self.model.model.startswith(("claude-", "gemini-")):
            request.insert(
                0,
                {
                    "role": "developer"
                    if self.model.model.startswith("gpt-")
                    else "system",
                    "content": self.system_prompt,
                },
            )
        self.last_plan = {"at": now, "input": snapshot}
        try:
            from coffeebench.models._retry import attempt_limit

            token = attempt_limit.set(1)
            try:
                response = self.model.query(request, tools=[self.plan_spec])
            finally:
                attempt_limit.reset(token)
            if (
                len(response.tool_calls) != 1
                or response.tool_calls[0].name != "submit_plan"
            ):
                raise ValueError("Expected exactly one submit_plan call")
            plan = response.tool_calls[0].input
            validate(plan, self.plan_spec.input_schema)
            calls = []
            for index, action in enumerate(plan["actions"]):
                args = json.loads(action["arguments_json"])
                spec = self.action_specs[action["name"]]
                schema = {**spec.input_schema, "additionalProperties": False}
                validate(args, schema)
                calls.append(
                    ToolCall(f"plan-{self.decision_calls}-{index}", spec.name, args)
                )
            self.memory = plan["memory"]
            self.pending.extend(calls)
            self.last_plan["plan"] = plan
        except Exception as exc:
            self.decision_errors += 1
            self.last_plan["error"] = f"{type(exc).__name__}: {exc}"[:1000]
            self.recent_results.append({"planning_error": self.last_plan["error"]})
        return (
            ModelResponse(tool_calls=[self.pending.popleft()])
            if self.pending
            else ModelResponse(content="No executable plan; waiting for next slot.")
        )

    def step_apply(self, response):
        if self.last_plan is not None:
            self.messages.append(
                {
                    "role": "user",
                    "content": json.dumps(
                        {"budget_decision": self.last_plan}, default=str
                    ),
                }
            )
            for msg in self.last_plan["input"]["messages"]:
                self.ba.marketplace.mark_message_read(self.name, msg["id"])
            self.last_plan = None
        result = super().step_apply(response)
        self.recent_results.append(result)
        observation = result.get("observation", {})
        if (
            result["action_name"] == "wait_for_next_day"
            or isinstance(observation, dict)
            and observation.get("status") == "error"
        ):
            self.pending.clear()
        return result
