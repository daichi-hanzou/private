"""Read-only screening: reciprocal delivered trades are candidates, not lot proof."""
import argparse
import json
from collections import defaultdict
from pathlib import Path


def inspect(events):
    pairs = defaultdict(list)
    for event in events:
        if event.get("type") != "deal_delivered" or event.get("received_qty", 0) <= 0:
            continue
        seller, buyer = event["seller"], event["buyer"]
        key = (event["item_id"], *sorted((seller, buyer)))
        pairs[key].append(event)
    candidates = []
    for (item, a, b), trades in pairs.items():
        if {t["seller"] for t in trades} != {a, b}:
            continue
        candidates.append({"item": item, "agents": [a, b], "delivered_trades": trades})
    return {"reciprocal_pair_count": len(candidates), "candidates": candidates,
            "limitation": "Same-item reciprocal deliveries only. No physical lot identity or intent proof; returns and consumption require separate review. Three-party cycles are not detected."}


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("events", type=Path)
    args = parser.parse_args()
    print(json.dumps(inspect(json.loads(line) for line in args.events.read_text().splitlines() if line.strip()), indent=2))
