"""Revalidate saved LLM responses and rebuild the trade report without API calls."""
import argparse
import copy
import json
from pathlib import Path
from coffeebench.trade_judge import VERSION, assess_response, build_packets, fingerprint, save_report
from coffeebench.trade_review import enrich_deals


def rebuild(saved,run=None):
    result=copy.deepcopy(saved)
    if run is not None and saved.get('settings',{}).get('source_sha256') not in {None,fingerprint(run)}:
        raise ValueError('Trajectory does not match the saved assessment source')
    packets={p['id']:p for p in build_packets(run)} if run is not None else {}
    for entry in result['reviews']:
        entry['packet']=packets.get(entry['packet']['id'],entry['packet'])
        entry['packet']['deals']=enrich_deals(entry['packet']['deals'],
                                            run.get('provenance',{}).get('events') if run else None)
        old=entry['assessment']
        raw=old.get('raw_response')
        if not raw and isinstance(old.get('verdict'),dict):
            raw=json.dumps(old['verdict'],ensure_ascii=False)
        if raw:
            entry.setdefault('previous_assessment',old)
            entry['assessment']=assess_response(raw,entry['packet'])
    result['validation_version']=VERSION
    return result


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('assessment',type=Path)
    p.add_argument('--trajectory',type=Path,help='Optional matching run.json to recover provenance delivery facts')
    p.add_argument('--output',type=Path)
    args=p.parse_args()
    output=args.output or args.assessment.with_name(args.assessment.stem+'.review.json')
    if any(output.resolve()==x.resolve() or output.with_suffix('.html').resolve()==x.resolve()
           for x in [args.assessment,args.trajectory] if x):
        p.error('Choose a separate output to preserve the inputs')
    data=json.loads(args.assessment.read_text(encoding='utf-8'))
    run=json.loads(args.trajectory.read_text(encoding='utf-8')) if args.trajectory else None
    try:
        result=rebuild(data,run)
    except ValueError as exc:
        p.error(str(exc))
    save_report(result,output)
    print(output)
    print(output.with_suffix('.html'))


if __name__=='__main__': main()
