"""Gate check: Blind / Investigator / Oracle roaster policies.

Purpose
-------
LLMを走らせる前の環境合格判定(P5ゲート)。
3体のロボットは「農園の選び方 (rank_farmers) 」だけが異なり、
発注量・発注タイミング・商社の使い方・値付けは完全に共通。

合格ライン:
  GATE1: mean(Oracle) - mean(Blind) >= 20% of |mean(Blind)|
  GATE2: (mean(Investigator) - mean(Blind)) / (mean(Oracle) - mean(Blind)) >= 0.5

前提:
  mini_coffee_env.py に debug_set_seed / debug_get_true_state パッチ適用済み
  (mini_coffee_env_patch.md を参照)
  環境変数 MINI_COFFEE_DEBUG_TOOLS=1 でサーバ起動

Usage:
  python gate_check_bio.py --seeds 100
"""

from __future__ import annotations

import argparse
import json
import math
import os
import re
import statistics
from dataclasses import dataclass, field

from ors.client import ORS

BASE_URL = os.getenv("MINI_COFFEE_ORS_URL", "http://localhost:8082")

# ---------------------------------------------------------------------------
# 共通パラメータ(3ロボットで完全に同一。ここを変えたら3体全部に効く)
# ---------------------------------------------------------------------------
TARGET_STOCK = {"standard": 14, "premium": 6}      # 目標在庫(約2日分)
REORDER_POINT = {"standard": 8, "premium": 4}      # これを下回ったら直販発注
EMERGENCY_POINT = {"standard": 5, "premium": 2}    # これを下回ったら商社で即時補充
OVERBOOK_FACTOR = 1.5                              # 未達を見込んだ発注倍率
INVESTIGATE_BUDGET = 3                             # Investigatorが初日に調べる軒数
UNKNOWN_RELIABILITY_PRIOR = 0.70                   # 開示情報がない農園の事前推定値
MIN_GAP_FOR_RATIO = 0.10                           # 分母がBlind利益の10%未満なら捕捉率を出さない


# ---------------------------------------------------------------------------
# 観測のパース(view_state / investigate の固定フォーマットに依存)
# ---------------------------------------------------------------------------
@dataclass
class Observation:
    day: int = 0
    cash: float = 0.0
    inventory: dict = field(default_factory=dict)          # item -> kg
    offers: dict = field(default_factory=dict)             # farmer -> {item: price}
    trader_price: dict = field(default_factory=dict)       # item -> price
    trader_stock: dict = field(default_factory=dict)       # item -> kg
    inbound: dict = field(default_factory=dict)            # item -> remaining kg (open contracts)


RE_DAY = re.compile(r"^Day (\d+) of (\d+)")
RE_CASH = re.compile(r"^Cash: \$([\d,.]+)")
RE_INV = re.compile(r"^\s+(standard|premium): (\d+) kg on hand")
RE_OFFER = re.compile(r"^\s+(\w+): standard \$([\d.]+)/kg, premium \$([\d.]+)/kg")
RE_TRADER_STOCK = re.compile(r"Trader inventory: standard (\d+) kg, premium (\d+) kg")
RE_TRADER_PRICE = re.compile(r"Trader offers: standard \$([\d.]+)/kg, premium \$([\d.]+)/kg")
RE_OPEN = re.compile(r"(\d+) kg (standard|premium) due day (\d+) \(remaining (\d+) kg\)")
RE_DELIV_RATE = re.compile(r"delivery_rate=([\d.]+)")


def parse_state(text: str) -> Observation:
    obs = Observation(inventory={}, offers={}, trader_price={}, trader_stock={}, inbound={"standard": 0, "premium": 0})
    in_offers = False
    for line in text.splitlines():
        if m := RE_DAY.match(line):
            obs.day = int(m.group(1))
        elif m := RE_CASH.match(line):
            obs.cash = float(m.group(1).replace(",", ""))
        elif m := RE_INV.match(line):
            obs.inventory[m.group(1)] = int(m.group(2))
        elif line.strip() == "Direct farmer spot offers today:":
            in_offers = True
        elif line.strip().startswith("Trader desk:"):
            in_offers = False
        elif in_offers and (m := RE_OFFER.match(line)):
            obs.offers[m.group(1)] = {"standard": float(m.group(2)), "premium": float(m.group(3))}
        elif m := RE_TRADER_STOCK.search(line):
            obs.trader_stock = {"standard": int(m.group(1)), "premium": int(m.group(2))}
        elif m := RE_TRADER_PRICE.search(line):
            obs.trader_price = {"standard": float(m.group(1)), "premium": float(m.group(2))}
        elif m := RE_OPEN.search(line):
            obs.inbound[m.group(2)] = obs.inbound.get(m.group(2), 0) + int(m.group(4))
    return obs


def parse_investigation(text: str):
    """returns delivery_rate or None (unknown / insufficient history)."""
    if m := RE_DELIV_RATE.search(text):
        return float(m.group(1))
    return None


def parse_final(text: str) -> dict:
    metrics = {}
    for line in text.splitlines():
        if line.startswith("Final value: $"):
            metrics["final_value"] = float(line.split("$", 1)[1])
        elif line.startswith("Profit: $"):
            metrics["profit"] = float(line.split("$", 1)[1].replace("+", ""))
        elif line.startswith("Investigation spend: $"):
            metrics["investigation_spend"] = float(line.split("$", 1)[1])
        elif line.startswith("Trader spend: $"):
            metrics["trader_spend"] = float(line.split("$", 1)[1])
        elif line.startswith("Direct spend: $"):
            metrics["direct_spend"] = float(line.split("$", 1)[1])
    return metrics


# ---------------------------------------------------------------------------
# ポリシー:共通ロジック + select だけ差し替え
# ---------------------------------------------------------------------------
class BasePolicy:
    name = "base"

    def __init__(self):
        self.reliability_estimate: dict[str, float] = {}

    # --- 差し替えポイント(これ以外は共通) ---
    def on_episode_start(self, call, obs: Observation) -> None:
        """調査・真値取得など、初日に1回だけ行う情報収集。"""

    def rank_farmers(self, obs: Observation, item: str) -> list[str]:
        """調達先の優先順位を返す。唯一の差別化点。"""
        raise NotImplementedError

    # --- 共通の1日の行動 ---
    def act(self, call, obs: Observation) -> None:
        for item in ("standard", "premium"):
            on_hand = obs.inventory.get(item, 0)
            pipeline = on_hand + obs.inbound.get(item, 0)

            # 直販発注(リオーダーポイント方式、未達見込みで上乗せ)
            if pipeline < REORDER_POINT[item]:
                need = TARGET_STOCK[item] - pipeline
                qty = max(1, math.ceil(need * OVERBOOK_FACTOR))
                ranked = self.rank_farmers(obs, item)
                if ranked:
                    farmer = ranked[0]
                    price = obs.offers[farmer][item]
                    if price * qty <= obs.cash * 0.5:  # 現金の半分までしか1回に使わない
                        call("buy_spot_direct", {"farmer_id": farmer, "item_id": item, "quantity_kg": qty})
                        obs.cash -= price * qty

            # 商社での緊急補充(在庫切れ回避、3体共通ルール)
            if on_hand < EMERGENCY_POINT[item]:
                shortfall = EMERGENCY_POINT[item] + 2 - on_hand
                qty = min(shortfall, obs.trader_stock.get(item, 0))
                if qty >= 1:
                    cost = obs.trader_price.get(item, 99.0) * qty
                    if cost <= obs.cash:
                        call("buy_from_trader", {"item_id": item, "quantity_kg": qty})
                        obs.cash -= cost

        # 値付けは3体とも一切いじらない(デフォルト価格のまま)= 交絡排除


class BlindPolicy(BasePolicy):
    """価格と公開情報だけ。調査しない。"""
    name = "blind"

    def rank_farmers(self, obs, item):
        return sorted(obs.offers, key=lambda f: obs.offers[f][item])


class InvestigatorPolicy(BasePolicy):
    """初日に安値上位k軒を有料調査し、価格÷推定信頼度で選ぶ。"""
    name = "investigator"

    def on_episode_start(self, call, obs):
        cheapest = sorted(obs.offers, key=lambda f: obs.offers[f]["standard"])[:INVESTIGATE_BUDGET]
        for farmer in cheapest:
            text = call("investigate_farmer", {"farmer_id": farmer})
            rate = parse_investigation(text)
            if rate is not None:
                self.reliability_estimate[farmer] = rate

    def rank_farmers(self, obs, item):
        def score(f):
            rel = self.reliability_estimate.get(f, UNKNOWN_RELIABILITY_PRIOR)
            return obs.offers[f][item] / max(rel, 0.05)
        return sorted(obs.offers, key=score)


class OraclePolicy(BasePolicy):
    """真の信頼度を裏口から取得して、価格÷真値で選ぶ(ズル)。"""
    name = "oracle"

    def on_episode_start(self, call, obs):
        text = call("debug_get_true_state", {})
        state = json.loads(text)
        self.reliability_estimate = {
            fid: max(info["fulfillment_reliability"], 0.05) for fid, info in state.items()
        }

    def rank_farmers(self, obs, item):
        def score(f):
            rel = self.reliability_estimate.get(f, UNKNOWN_RELIABILITY_PRIOR)
            return obs.offers[f][item] / rel
        return sorted(obs.offers, key=score)


# ---------------------------------------------------------------------------
# 実行ループ
# ---------------------------------------------------------------------------
def run_episode(session, policy: BasePolicy, seed: int, total_days: int) -> dict:
    def call(tool: str, args: dict) -> str:
        result = session.call_tool(tool, args)
        return "\n".join(block.text for block in result.blocks)

    call("debug_set_seed", {"seed": seed})
    obs = parse_state(call("view_state", {}))
    policy.reliability_estimate = {}
    policy.on_episode_start(call, obs)
    obs = parse_state(call("view_state", {}))  # 調査支出後の現金を反映

    for _ in range(total_days):
        policy.act(call, obs)
        call("advance_day", {})
        obs = parse_state(call("view_state", {}))

    final = parse_final(call("finish_episode", {}))
    final["seed"] = seed
    final["policy"] = policy.name
    return final


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--seeds", type=int, default=100)
    parser.add_argument("--total-days", type=int, default=int(os.getenv("MINI_COFFEE_TOTAL_DAYS", "20")))
    parser.add_argument("--csv", type=str, default="gate_check_results.csv")
    args = parser.parse_args()

    client = ORS(base_url=BASE_URL)
    policies = [BlindPolicy, InvestigatorPolicy, OraclePolicy]
    rows: list[dict] = []

    try:
        env = client.environment("minicoffeeenv")
        task = env.list_tasks(split="train")[0]
        for policy_cls in policies:
            for seed in range(args.seeds):
                with env.session(task=task) as session:
                    session.get_prompt()
                    rows.append(run_episode(session, policy_cls(), seed, args.total_days))
    finally:
        client.close()

    # --- 集計 ---
    with open(args.csv, "w") as f:
        keys = ["policy", "seed", "profit", "final_value", "investigation_spend", "direct_spend", "trader_spend"]
        f.write(",".join(keys) + "\n")
        for row in rows:
            f.write(",".join(str(row.get(k, "")) for k in keys) + "\n")

    def stats(name: str):
        vals = [r["profit"] for r in rows if r["policy"] == name]
        mean = statistics.mean(vals)
        se = statistics.stdev(vals) / math.sqrt(len(vals)) if len(vals) > 1 else 0.0
        return mean, se

    blind_mean, blind_se = stats("blind")
    inv_mean, inv_se = stats("investigator")
    oracle_mean, oracle_se = stats("oracle")

    info_gap = oracle_mean - blind_mean
    gap_pct = info_gap / max(abs(blind_mean), 1.0)

    print("\n================ GATE CHECK ================")
    print(f"seeds per policy : {args.seeds}")
    print(f"Blind        profit: {blind_mean:9.2f}  (±{1.96 * blind_se:.2f})")
    print(f"Investigator profit: {inv_mean:9.2f}  (±{1.96 * inv_se:.2f})")
    print(f"Oracle       profit: {oracle_mean:9.2f}  (±{1.96 * oracle_se:.2f})")
    print(f"\nOracle - Blind gap : {info_gap:9.2f}  ({gap_pct:+.1%} of |Blind|)")

    gate1 = gap_pct >= 0.20
    print(f"GATE1 (gap >= 20%)          : {'PASS' if gate1 else 'FAIL'}")

    if abs(info_gap) < MIN_GAP_FOR_RATIO * max(abs(blind_mean), 1.0):
        print("GATE2 (capture >= 50%)      : SKIPPED (分母が小さすぎるため捕捉率は無意味)")
    else:
        capture = (inv_mean - blind_mean) / info_gap
        gate2 = capture >= 0.5
        print(f"Investigator capture rate   : {capture:.1%}")
        print(f"GATE2 (capture >= 50%)      : {'PASS' if gate2 else 'FAIL'}")

    if not gate1:
        print("\n[次の一手] ギャップ不足のときの調整順:")
        print("  1. _fulfillment_limit のティア圧縮を解消(patch案A/B)")
        print("  2. 返金率を理由別に引き下げ(PR-2の 90/65/35)")
        print("  3. FARMER_PROFILES の reliability 分散を広げる")
    print("============================================\n")


if __name__ == "__main__":
    main()
