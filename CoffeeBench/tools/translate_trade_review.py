"""Add Japanese message translations to a saved trade report using Azure."""
import argparse
import copy
import json
import os
from pathlib import Path
from dotenv import load_dotenv
from coffeebench.trade_judge import fingerprint, save_report
from coffeebench.trade_translation import PROMPT, records, translate_batch


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('report',type=Path)
    p.add_argument('--output',type=Path)
    p.add_argument('--resume',action='store_true',help='Reuse translations for identical source and settings')
    p.add_argument('--effort',choices=['low','medium','high','off'],default='low')
    p.add_argument('--batch-size',type=int,default=6)
    p.add_argument('--max-batch-chars',type=int,default=16000)
    args=p.parse_args()
    if args.batch_size<1 or args.max_batch_chars<1: p.error('Batch limits must be positive')
    path=args.output or args.report.with_name(args.report.stem+'.ja.json')
    if args.report.resolve() in {path.resolve(),path.with_suffix('.html').resolve()}:
        p.error('Use a separate output to preserve the original report')
    source=json.loads(args.report.read_text(encoding='utf-8'))
    report=copy.deepcopy(source)
    load_dotenv()
    settings=dict(prompt_sha256=fingerprint(PROMPT),endpoint=os.getenv('AZURE_OPENAI_ENDPOINT',''),
                  deployment=os.getenv('AZURE_OPENAI_DEPLOYMENT',''),effort=args.effort,
                  source_sha256=fingerprint(source))
    # An explicit translation run uses its own cache; display-only annotations
    # copied from a different source are not silently treated as this model's work.
    translations={}
    if args.resume and path.exists():
        old=json.loads(path.read_text(encoding='utf-8'))
        if old.get('translation_settings')!=settings:
            p.error('Source or translation settings changed; choose another output')
        translations=old.get('translations_ja',{})
    report['translation_settings']=settings
    report['translations_ja']=translations
    messages=records(report)
    pending=[m for k,m in messages.items() if translations.get(k,{}).get('status') not in {'translated','needs_review'}]
    batches=[];batch=[];size=0
    for m in pending:
        length=len(json.dumps(m,ensure_ascii=False))
        if length>args.max_batch_chars:
            translations[m['key']]=dict(status='too_large',error='本文を省略せず保持しています。--max-batch-chars を増やして再開してください。')
            continue
        if batch and (len(batch)>=args.batch_size or size+length>args.max_batch_chars):
            batches.append(batch);batch=[];size=0
        batch.append(m);size+=length
    if batch: batches.append(batch)
    save_report(report,path)
    model=None
    try:
        if batches:
            from coffeebench.models.azure_openai_model import AzureOpenAIModel
            model=AzureOpenAIModel(args.effort)
            model.max_tokens=12000
        for i,batch in enumerate(batches,1):
            translations.update(translate_batch(model,batch))
            report['translation_usage_this_invocation']=model.get_usage_stats()
            save_report(report,path)
            print(f'Translation batch {i}/{len(batches)} saved',flush=True)
    finally:
        if model:
            model.client.close();model.credential.close()
    print(path);print(path.with_suffix('.html'))
    if any(t.get('status') in {'error','too_large'} for t in translations.values()): raise SystemExit(1)


if __name__=='__main__': main()
