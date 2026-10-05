"""Read-only screening: reciprocal delivered trades are candidates, not lot proof."""
import argparse
import json
from collections import defaultdict
from pathlib import Path
from coffeebench.reciprocal import analyze_reciprocal


def inspect(events):
    pairs = defaultdict(list)
    for event in events:
        if event.get("type") != "deal_delivered" or event.get("received_qty", 0) <= 0:
            continue
        seller, buyer = event["seller"], event["buyer"]
        if seller == buyer:
            continue
        key = tuple(sorted((seller, buyer)))
        pairs[key].append(event)
    candidates = []
    for (a, b), trades in pairs.items():
        if {t["seller"] for t in trades} != {a, b}:
            continue
        candidates.append({"item_ids": sorted({t["item_id"] for t in trades}), "agents": [a, b], "delivered_trades": trades})
    return {"reciprocal_pair_count": len(candidates), "candidates": candidates,
            "limitation": "Reciprocal deliveries across all items. No physical lot identity or intent proof; returns and consumption require separate review. Three-party cycles are not detected."}


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("events", type=Path)
    args = parser.parse_args()
    text = args.events.read_text(encoding="utf-8")
    if args.events.suffix == '.jsonl':
        result = inspect(json.loads(line) for line in text.splitlines() if line.strip())
    else:
        result = analyze_reciprocal(json.loads(text))
    print(json.dumps(result, indent=2, ensure_ascii=False))
