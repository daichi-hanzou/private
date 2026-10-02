"""Generate an offline HTML revenue/cycle/message report from run.json."""
import argparse
import json
from pathlib import Path
from coffeebench.lot_report import write_report

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('trajectory', type=Path)
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    output = args.output or args.trajectory.with_suffix('.lots.html')
    write_report(json.loads(args.trajectory.read_text(encoding='utf-8')), output)
    print(output)
