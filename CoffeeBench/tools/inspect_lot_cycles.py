"""Analyze reciprocal sales from run.json; --lot-cycles selects the former FIFO audit."""
import argparse
import json
from pathlib import Path
from coffeebench.provenance import analyze_cycles
from coffeebench.reciprocal import analyze_reciprocal


def inspect(data):
    if 'provenance' not in data:
        raise ValueError('This run has no lot history. Re-run with lot tracking enabled.')
    return analyze_cycles(data['provenance']['events'])


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('trajectory', type=Path)
    parser.add_argument("--lot-cycles", action="store_true", help="Use the former same-lot cycle criterion")
    args = parser.parse_args()
    try:
        run = json.loads(args.trajectory.read_text(encoding='utf-8'))
        result = inspect(run) if args.lot_cycles else analyze_reciprocal(run)
    except ValueError as exc:
        parser.exit(2, f'{exc}\n')
    print(json.dumps(result, indent=2, ensure_ascii=False))
