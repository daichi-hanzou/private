"""Prepare trade evidence offline; --judge opts into sequential Azure LLM reviews."""
import argparse
import json
from pathlib import Path
from coffeebench.trade_judge import VERSION, PROMPT, build_packets, fingerprint, review_packet, save_report


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('trajectory',type=Path)
    parser.add_argument('--output',type=Path)
    parser.add_argument('--judge',action='store_true',help='Call Azure using existing endpoint/deployment and DefaultAzureCredential')
    parser.add_argument('--effort',choices=['low','medium','high','off'],default='low')
    parser.add_argument('--max-packet-chars',type=int,default=120000)
    parser.add_argument('--resume',action='store_true',help='Reuse completed reviews with identical input, prompt and deployment')
    args=parser.parse_args()
    if args.max_packet_chars<=0:
        parser.error('--max-packet-chars must be positive')
    run=json.loads(args.trajectory.read_text(encoding='utf-8'))
    path=args.output or args.trajectory.with_suffix('.trade_judgments.json')
    if path.resolve()==args.trajectory.resolve() or path.with_suffix('.html').resolve()==args.trajectory.resolve():
        parser.error('Output must not overwrite the input trajectory')
    packets=build_packets(run)
    model=None
    from dotenv import load_dotenv
    load_dotenv()
    import os
    settings=dict(version=VERSION,prompt_sha256=fingerprint(PROMPT),effort=args.effort,
                  endpoint=os.getenv('AZURE_OPENAI_ENDPOINT',''),deployment=os.getenv('AZURE_OPENAI_DEPLOYMENT',''),
                  source_sha256=fingerprint(run),max_packet_chars=args.max_packet_chars)
    previous={}
    if args.resume and path.exists():
        old=json.loads(path.read_text(encoding='utf-8'))
        if old.get('settings')!=settings:
            parser.error('Resume settings/input differ; use --output with a new filename. To rebuild old results without API calls, use tools.render_trade_review')
        previous={e['packet']['id']:e for e in old['reviews'] if e['assessment']['status'] in {'reviewed','needs_review'}}
    if args.judge and packets:
        from coffeebench.models.azure_openai_model import AzureOpenAIModel
        model=AzureOpenAIModel(args.effort)
    report=dict(settings=settings,source=str(args.trajectory),reviews=[])
    try:
        for packet in packets:
            prior=previous.get(packet['id']) if args.judge else None
            if prior and prior['packet']==packet:
                entry=prior
            else:
                entry=dict(packet=packet,assessment=review_packet(model,packet,args.max_packet_chars)
                           if model else dict(status='not_reviewed'))
            report['reviews'].append(entry)
            if model:
                report['usage_this_invocation']=model.get_usage_stats()
            save_report(report,path)
            print(packet['id'],entry['assessment']['status'],flush=True)
        save_report(report,path)
    finally:
        if model:
            model.client.close()
            model.credential.close()
    print(path)
    print(path.with_suffix('.html'))
    if any(e['assessment']['status'] in {'error','too_large'} for e in report['reviews']):
        raise SystemExit(1)


if __name__=='__main__':
    main()
