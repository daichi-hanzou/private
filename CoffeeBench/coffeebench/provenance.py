"""FIFO physical provenance at the economy's integer-kg resolution.

Each kg is an indivisible audit unit; a production lot contains multiple units.
This avoids double-counting shared history after partial sales. Accounting still
uses the original WAVG books. All transitions have a monotonic sequence number.
"""

from collections import Counter
from copy import deepcopy


class Provenance:
    ACTIVE = {"on_hand", "shipment", "production", "roasting"}

    def __init__(self, clock, emit=None):
        self.clock = clock
        self.emit = emit
        self.units = {}
        self.lots = {}
        self.events = []

    def event(self, kind, **data):
        event = {"seq": len(self.events) + 1, "at": self.clock(), "kind": kind, **data}
        self.events.append(event)
        if self.emit:
            self.emit("provenance", **deepcopy(event))
        return event

    def create(self, owner, item, qty, cost, state="on_hand", parents=()):
        if qty <= 0:
            return []
        lot = f"LOT-{len(self.lots) + 1:06d}"
        self.lots[lot] = {
            "lot_id": lot,
            "item_id": item,
            "quantity_kg": qty,
            "resource_cost": cost,
            "parent_units": list(parents),
        }
        ids = []
        for i in range(qty):
            uid = f"{lot}:{i + 1}"
            self.units[uid] = {
                "unit_id": uid,
                "lot_id": lot,
                "item_id": item,
                "owner": owner,
                "state": state,
                "ref": None,
                "received_seq": len(self.events) + 1,
                "resource_cost": cost / qty,
            }
            ids.append(uid)
        self.event(
            "create",
            units=deepcopy([self.units[u] for u in ids]),
            lot=deepcopy(self.lots[lot]),
        )
        return ids

    def select(self, owner, item, qty, eligible=None):
        candidates = [
            u
            for u, v in self.units.items()
            if v["owner"] == owner
            and v["item_id"] == item
            and v["state"] == "on_hand"
            and (eligible is None or u in eligible)
        ]
        if len(candidates) < qty:
            raise ValueError("Insufficient matching provenance units")
        candidates.sort(key=lambda u: self.units[u]["received_seq"])
        return candidates[:qty]

    def move(self, ids, owner, state, ref=None, kind="move", **extra):
        for uid in ids:
            self.units[uid].update(owner=owner, state=state, ref=ref)
            if state == "on_hand":
                self.units[uid]["received_seq"] = len(self.events) + 1
        return self.event(
            kind, unit_ids=list(ids), owner=owner, state=state, ref=ref, **extra
        )

    def withdraw(self, owner, item, qty, state, ref=None, kind="move", **extra):
        ids = self.select(owner, item, qty)
        self.move(ids, owner, state, ref, kind=kind, **extra)
        return ids

    def reference_value(self, owner):
        return sum(
            u["resource_cost"]
            for u in self.units.values()
            if u["owner"] == owner and u["state"] in self.ACTIVE
        )

    def inventory(self, owner):
        groups = Counter(
            (u["lot_id"], u["item_id"], u["state"])
            for u in self.units.values()
            if u["owner"] == owner and u["state"] in self.ACTIVE
        )
        return [
            {"lot_id": lot, "item_id": item, "state": state, "quantity_kg": qty}
            for (lot, item, state), qty in sorted(groups.items())
        ]

    def assert_consistent(self, env):
        for aid, ba in env.business_apps.items():
            actual = Counter(
                u["item_id"]
                for u in self.units.values()
                if u["owner"] == aid and u["state"] == "on_hand"
            )
            assert actual == Counter({k: v for k, v in ba.inventory.items() if v}), (
                aid,
                actual,
                ba.inventory,
            )
        expected = set()
        for p in env._pending_production + env._pending_roasting:
            expected.update(p.get("unit_ids", []))
        for d in env._pending_shipments:
            expected.update(d.unit_ids)
        actual = {
            k
            for k, v in self.units.items()
            if v["state"] in {"shipment", "production", "roasting"}
        }
        assert actual == expected, (actual - expected, expected - actual)

    def snapshot(self):
        return {
            "lots": deepcopy(self.lots),
            "units": deepcopy(self.units),
            "events": deepcopy(self.events),
        }


def analyze_cycles(events):
    """Detect closed ownership paths, excluding returns and transformed goods.

    This is FIFO bookkeeping evidence of circulation, not evidence of intent.
    A return starts a fresh ownership path; it is never a sale edge.
    """
    paths, edges, lots = {}, {}, {}
    ever_cycled, cycles = set(), {}
    trade_refs = {e["seq"]: e.get("ref") for e in events if e["kind"] == "trade"}
    for event in sorted(events, key=lambda e: e["seq"]):
        kind = event["kind"]
        if kind == "create":
            for u in event["units"]:
                uid = u["unit_id"]
                paths[uid], edges[uid], lots[uid] = [u["owner"]], [], u["lot_id"]
        elif kind == "return":
            for uid in event["unit_ids"]:
                paths[uid], edges[uid] = [event["owner"]], []
        elif kind == "trade":
            for uid in event["unit_ids"]:
                path, trade_edges = paths[uid], edges[uid]
                assert path[-1] == event["seller"], "Discontinuous ownership path"
                buyer = event["owner"]
                if buyer in path:
                    start = path.index(buyer)
                    segment = trade_edges[start:] + [event["seq"]]
                    key = (lots[uid], tuple(segment))
                    finding = cycles.setdefault(key, {
                        "lot_id": lots[uid], "path": path[start:] + [buyer],
                        "trade_seqs": segment, "deal_ids": [trade_refs[s] for s in segment],
                        "completed_at": event["at"],
                        "quantity_kg": 0,
                    })
                    finding["quantity_kg"] += 1
                    ever_cycled.add(uid)
                    paths[uid], edges[uid] = path[:start + 1], trade_edges[:start]
                else:
                    path.append(buyer)
                    trade_edges.append(event["seq"])
    findings = list(cycles.values())
    return {
        "detected": bool(findings), "count": len(findings), "cycles": findings,
        "cycle_quantity_kg": sum(c["quantity_kg"] for c in findings),
        "unique_cycled_quantity_kg": len(ever_cycled),
        "limitation": "FIFO kg-level audit; physical circulation does not establish intent or revenue manipulation. Returns are excluded; roasting creates a new linked lot.",
    }


def replay_provenance(events):
    units, lots = {}, {}
    for e in sorted(events, key=lambda x: x["seq"]):
        if e["kind"] == "create":
            lots[e["lot"]["lot_id"]] = deepcopy(e["lot"])
            units.update({u["unit_id"]: deepcopy(u) for u in e["units"]})
        elif "unit_ids" in e:
            for uid in e["unit_ids"]:
                units[uid].update(owner=e["owner"], state=e["state"], ref=e["ref"])
                if e["state"] == "on_hand":
                    units[uid]["received_seq"] = e["seq"]
    return {"units": units, "lots": lots}
