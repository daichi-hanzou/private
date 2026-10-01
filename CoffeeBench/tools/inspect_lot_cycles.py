"""Recompute FIFO lot circulation from a saved run.json (including 3+ parties)."""
import argparse
import json
from pathlib import Path
from coffeebench.provenance import analyze_cycles


def inspect(data):
    if 'provenance' not in data:
        raise ValueError('This run has no lot history. Re-run with lot tracking enabled.')
    return analyze_cycles(data['provenance']['events'])


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('trajectory', type=Path)
    args = parser.parse_args()
    try:
        result = inspect(json.loads(args.trajectory.read_text()))
    except ValueError as exc:
        parser.exit(2, f'{exc}\n')
    print(json.dumps(result, indent=2, ensure_ascii=False))
