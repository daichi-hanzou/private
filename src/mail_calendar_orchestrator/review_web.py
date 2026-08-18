from __future__ import annotations

import json
from http.server import BaseHTTPRequestHandler, HTTPServer
from urllib.parse import parse_qs, urlparse

from .review_models import HumanVerdictInput
from .review_service import ClassificationReviewService


def serve_review_ui(
    service: ClassificationReviewService, *, host: str = "127.0.0.1", port: int = 8791,
) -> None:
    class Handler(BaseHTTPRequestHandler):
        def do_GET(self) -> None:  # noqa: N802
            parsed = urlparse(self.path)
            if parsed.path == "/":
                self._html(_INDEX_HTML)
            elif parsed.path == "/api/meta":
                self._json(200, {
                    "current_version": service.review_version,
                    "reviewer_model": service.reviewer.model_name,
                    "versions": service.available_versions(),
                })
            elif parsed.path == "/api/cases":
                query = parse_qs(parsed.query)
                queue_only = query.get("queue_only", ["0"])[0] != "0"
                limit = int(query.get("limit", ["200"])[0])
                all_versions = query.get("all_versions", ["0"])[0] != "0"
                version = query.get("version", [None])[0]
                self._json(200, [
                    {**_jsonable_case(item), "subject": service.case_subject(item)}
                    for item in service.list_cases(
                        queue_only=queue_only, limit=limit, version=version,
                        all_versions=all_versions,
                    )
                ])
            elif parsed.path.startswith("/api/cases/"):
                self._json(200, service.get_case_detail(
                    parsed.path.removeprefix("/api/cases/")
                ))
            else:
                self._json(404, {"error": "not found"})

        def do_POST(self) -> None:  # noqa: N802
            parsed = urlparse(self.path)
            length = int(self.headers.get("Content-Length", "0"))
            if length > 1024 * 1024:
                self._json(413, {"error": "request too large"})
                return
            payload = json.loads(self.rfile.read(length) or b"{}")
            if parsed.path.endswith("/chat"):
                case_id = parsed.path.removeprefix("/api/cases/").removesuffix("/chat")
                try:
                    reply = service.send_chat_message(
                        case_id, str(payload.get("message") or ""),
                        human_comment=str(payload.get("human_comment") or ""),
                        human_verdict=(
                            str(payload["human_verdict"])
                            if payload.get("human_verdict") else None
                        ),
                        final_classification=(
                            str(payload["final_classification"])
                            if payload.get("final_classification") else None
                        ),
                        recommended_change_target=(
                            str(payload["recommended_change_target"])
                            if payload.get("recommended_change_target") else None
                        ),
                    )
                except (RuntimeError, ValueError) as exc:
                    self._json(409, {"error": str(exc)})
                    return
                self._json(200, {"reply": reply})
            elif parsed.path.endswith("/verdict"):
                case_id = parsed.path.removeprefix("/api/cases/").removesuffix("/verdict")
                record = service.save_verdict(case_id, HumanVerdictInput(
                    human_verdict=str(payload.get("human_verdict")),
                    final_classification=str(payload.get("final_classification") or ""),
                    lesson_summary=str(payload.get("lesson_summary") or ""),
                    human_comment=str(payload.get("human_comment") or ""),
                    recommended_change_target=str(
                        payload.get("recommended_change_target") or "none"
                    ),
                ))
                self._json(200, _jsonable_case(record))
            else:
                self._json(404, {"error": "not found"})

        def log_message(self, format: str, *args: object) -> None:
            del format, args

        def _json(self, status: int, payload: object) -> None:
            body = json.dumps(payload, ensure_ascii=False).encode()
            self.send_response(status)
            self.send_header("Content-Type", "application/json; charset=utf-8")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

        def _html(self, body: str) -> None:
            encoded = body.encode()
            self.send_response(200)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.send_header("Content-Length", str(len(encoded)))
            self.end_headers()
            self.wfile.write(encoded)

    HTTPServer((host, port), Handler).serve_forever()


def _jsonable_case(record: object) -> dict[str, object]:
    value = dict(vars(record))
    for key in ("created_at", "updated_at", "human_reviewed_at"):
        item = value.get(key)
        value[key] = item.isoformat() if item is not None else None
    return value


_INDEX_HTML = r"""<!doctype html><html lang="en"><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Classification Review</title><style>
:root{--bg:#f3f5f7;--panel:#fff;--ink:#17212b;--muted:#687482;--line:#dde3e8;--accent:#2563eb;--good:#138a5b;--warn:#a96500;--bad:#b63a45;--shadow:0 10px 30px #1d2b3912}*{box-sizing:border-box}body{margin:0;font-family:Inter,system-ui,sans-serif;background:var(--bg);color:var(--ink)}.app{display:grid;grid-template-columns:340px minmax(0,1fr);min-height:100vh}.sidebar{position:sticky;top:0;height:100vh;overflow:auto;padding:24px 16px;border-right:1px solid var(--line);background:#f8fafc}.main{padding:32px;max-width:1100px;width:100%;margin:auto}.brand{font-size:20px;font-weight:750;margin:0 8px 18px}.brand small{display:block;color:var(--muted);font-size:12px;font-weight:500;margin-top:4px}.filter,select,textarea,button{width:100%;font:inherit;padding:11px;border:1px solid var(--line);border-radius:10px;background:#fff}.filter{margin-bottom:16px}.case{padding:14px;border:1px solid transparent;border-radius:12px;margin-bottom:8px;cursor:pointer}.case:hover,.case.active{background:#fff}.case.active{border-color:#b9cdfa;box-shadow:var(--shadow)}.case-title{font-weight:650;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}.case-flow{font-size:12px;color:var(--muted);margin-top:7px}.card{background:var(--panel);border:1px solid var(--line);border-radius:16px;padding:22px;box-shadow:var(--shadow);margin-bottom:18px}.hero h1{font-size:24px;margin:8px 0}.muted{color:var(--muted);font-size:13px}.metrics{display:grid;grid-template-columns:repeat(4,1fr);gap:12px;margin-top:20px}.metric{padding:14px;background:#f8fafc;border-radius:12px}.metric label,label{display:block;font-size:11px;color:var(--muted);font-weight:650;margin-bottom:6px}.metric strong{display:block;overflow-wrap:anywhere}.badge{display:inline-flex;padding:4px 9px;border-radius:99px;background:#edf1f5;font-size:12px;font-weight:650}.badge.disagreement{background:#fff0f1;color:var(--bad)}.badge.uncertain{background:#fff6df;color:var(--warn)}.badge.resolved{background:#e7f7f0;color:var(--good)}.email-body{white-space:pre-wrap;line-height:1.65;max-height:360px;overflow:auto;padding:18px;background:#f8fafc;border-radius:12px;margin-top:12px}.assessment{border-left:4px solid var(--accent)}.grid{display:grid;grid-template-columns:1fr 1fr;gap:14px}.review-card{border:2px solid #b9cdfa}textarea{resize:vertical;min-height:120px}.actions{display:grid;grid-template-columns:auto auto 1fr auto;gap:10px;margin-top:16px}.actions button{cursor:pointer}.primary{background:var(--accent);color:#fff;border-color:var(--accent)}.save-state{color:var(--good);align-self:center;font-size:13px}.ask{margin-top:18px;width:auto}.chat-panel{margin-top:14px;padding:16px;background:#f8fafc;border-radius:12px}.chat-log{display:flex;flex-direction:column;gap:10px;max-height:320px;overflow:auto;margin-bottom:12px}.bubble{padding:11px 13px;border-radius:12px;max-width:88%;white-space:pre-wrap}.bubble.user{align-self:flex-end;background:#dfeaff}.bubble.assistant{align-self:flex-start;background:#fff;border:1px solid var(--line)}.chat-send{display:grid;grid-template-columns:1fr 100px;gap:10px}.chat-send textarea{min-height:72px}details{background:#fff;border:1px solid var(--line);border-radius:12px;padding:14px 18px;margin-bottom:12px}summary{cursor:pointer;font-weight:650}pre{white-space:pre-wrap;overflow-wrap:anywhere;background:#f8fafc;padding:14px;border-radius:10px;max-height:350px;overflow:auto}.empty{text-align:center;padding:80px;color:var(--muted)}@media(max-width:800px){.app{grid-template-columns:1fr}.sidebar{position:relative;height:auto;max-height:320px}.main{padding:18px}.metrics,.grid{grid-template-columns:1fr 1fr}.actions{grid-template-columns:1fr 1fr}}
</style><body><div class="app"><aside class="sidebar"><div class="brand">Classification Review<small>Teach the system with human judgment.</small><small id="currentReview"></small></div><label>Review version</label><select id="version" class="filter"></select><label>Reviewer status</label><select id="reviewerStatus" class="filter"><option value="all">All reviewer statuses</option><option value="agree">Agree</option><option value="disagreement">Disagreement</option><option value="uncertain">Uncertain</option></select><label>Human review status</label><select id="humanStatus" class="filter"><option value="unresolved">Unresolved</option><option value="resolved">Resolved</option><option value="all">All human statuses</option></select><div id="cases"></div></aside><main class="main"><div id="detail" class="empty">Select a review case to begin.</div></main></div><script>
const casesEl=document.querySelector('#cases'),detailEl=document.querySelector('#detail'),reviewerStatusEl=document.querySelector('#reviewerStatus'),humanStatusEl=document.querySelector('#humanStatus'),versionEl=document.querySelector('#version');const classes=['calendar_candidate','transactional','security_notification','ignored','invalid','clarification_required'];let allCases=[],visibleCases=[],currentId=null,reviewMeta=null;
function applyFilters(){renderCases();if(!visibleCases.some(x=>x.review_case_id===currentId)&&visibleCases.length)loadDetail(visibleCases[0].review_case_id)}
reviewerStatusEl.onchange=applyFilters;humanStatusEl.onchange=applyFilters;
versionEl.onchange=()=>{currentId=null;loadCases()};
async function initialize(){reviewMeta=await(await fetch('/api/meta')).json();document.querySelector('#currentReview').textContent=`Reviewer model: ${reviewMeta.reviewer_model} · Review version: ${reviewMeta.current_version}`;versionEl.innerHTML=[`<option value="${esc(reviewMeta.current_version)}">${esc(reviewMeta.current_version)} (current)</option>`,...reviewMeta.versions.filter(x=>x!==reviewMeta.current_version).map(x=>`<option value="${esc(x)}">${esc(x)}</option>`),'<option value="*">all versions</option>'].join('');await loadCases()}
async function loadCases(){const y=document.querySelector('.sidebar').scrollTop,v=versionEl.value;const query=v==='*'?'all_versions=1':`version=${encodeURIComponent(v)}`;allCases=await(await fetch(`/api/cases?queue_only=0&limit=500&${query}`)).json();renderCases();document.querySelector('.sidebar').scrollTop=y}
function renderCases(){const reviewer=reviewerStatusEl.value,human=humanStatusEl.value;visibleCases=allCases.filter(x=>(reviewer==='all'||x.review_status===reviewer)&&(human==='all'||(human==='unresolved'&&x.human_review_status==='pending')||(human==='resolved'&&x.human_review_status==='resolved')));casesEl.innerHTML='';visibleCases.forEach(x=>{const d=document.createElement('div');d.className='case'+(x.review_case_id===currentId?' active':'');d.innerHTML=`<div class="case-title">${esc(x.subject||'(No subject)')}</div><div class="case-flow">${esc(displayClass(x.final_classification))} → ${esc(displayClass(x.suggested_classification))}</div><div><span class="badge ${esc(x.review_status)}">${esc(x.review_status)}</span> <span class="badge ${esc(x.human_review_status)}">${x.human_review_status==='resolved'?'Resolved':'Unresolved'}</span></div>`;d.onclick=()=>loadDetail(x.review_case_id);casesEl.appendChild(d)})}
async function loadDetail(id){currentId=id;renderCases();detailEl.className='empty';detailEl.textContent='Loading case…';let response,d;try{response=await fetch(`/api/cases/${encodeURIComponent(id)}`);if(!response.ok)throw new Error(`HTTP ${response.status}`);d=await response.json()}catch(error){detailEl.innerHTML=`<div class="card"><strong>Unable to load this case.</strong><p class="muted">${esc(error.message)}</p></div>`;return}const c=d.context,k=d.case,s=d.event_snapshot||{},selected=normalizeClass(k.human_final_classification||k.final_classification||'');detailEl.className='';detailEl.innerHTML=`<section class="card hero"><span class="badge ${esc(k.review_status)}">${esc(k.review_status)}</span><h1>${esc(c.subject||k.subject||'(No subject)')}</h1><div class="muted">Received ${esc(c.received_at||'Not recorded')}</div><div class="metrics">${metric('Original',displayClass(c.original_classification))}${metric('Reviewer',displayClass(k.suggested_classification))}${metric('Review status',k.review_status)}${metric('Confidence',Number(k.confidence).toFixed(2))}</div></section><section class="card"><h2>📧 Email</h2><strong>Subject</strong><div>${esc(c.subject||'-')}</div><div class="email-body">${esc(c.analysis_text||'No body text available.')}</div></section><section class="card assessment"><h2>🤖 Reviewer assessment</h2><div class="grid"><div><span class="muted">Suggested classification</span><br><strong>${esc(displayClass(k.suggested_classification))}</strong></div><div><span class="muted">Issue</span><br><strong>${esc(k.issue_type||'-')}</strong></div></div><p><span class="muted">Reason</span><br>${esc(k.reason_summary||'-')}</p></section><details><summary>Decision trace</summary><pre>${esc(JSON.stringify({qwen_proposal:c.original_classification,validator_result:c.validator_result,deterministic_corrections:c.deterministic_corrections,final_classification:c.final_classification,line_notification:c.line_notification,approval:c.approval,calendar_outcome:c.calendar_execution},null,2))}</pre></details><details><summary>Technical details</summary><pre>${esc(JSON.stringify({review_id:k.review_case_id,provider:c.provider,canonical_message_id:c.message_id,correlation_id:s.correlation_id,source_jsonl_path:k.source_jsonl_path,reviewer_model:k.reviewer_model,review_version:k.review_version},null,2))}</pre></details><section class="card review-card"><h2>Human Review</h2><div class="grid"><div><label>Human verdict</label><select id="verdict"><option value="original_correct">Original correct</option><option value="reviewer_correct">Reviewer correct</option><option value="modified">Modified</option><option value="unresolved">Unresolved</option></select></div><div><label>Final classification</label><select id="final">${classes.map(x=>`<option ${x===selected?'selected':''}>${x}</option>`).join('')}</select></div><div><label>Recommended change target</label><select id="target"><option value="none">None</option><option value="qwen_prompt">Qwen prompt</option><option value="validator">Validator</option><option value="deterministic_rule">Deterministic rule</option><option value="test">Test</option></select></div></div><label style="margin-top:14px">Human comment</label><textarea id="comment" placeholder="Why is this the right judgment?">${esc(k.human_comment||'')}</textarea><button class="ask" onclick="askReviewer()">Ask Reviewer</button><div id="reviewerChat" class="chat-panel" hidden><div id="chatLog" class="chat-log">${chatHtml(d.chat_history||[])}</div><div class="chat-send"><textarea id="chatInput" placeholder="Ask a question about this case"></textarea><button class="primary" onclick="sendReviewer()">Send</button></div></div><div class="actions"><button onclick="navigate(-1)">← Previous</button><button onclick="navigate(1)">Next →</button><span id="state" class="save-state">${k.human_review_status==='resolved'?'Resolved':''}</span><button class="primary" onclick="saveNext()">Save & Next</button></div></section>`;document.querySelector('#verdict').value=k.human_verdict||'unresolved';document.querySelector('#target').value=k.recommended_change_target||'none'}
function metric(label,value){return `<div class="metric"><label>${label}</label><strong>${esc(value||'-')}</strong></div>`}function navigate(n){const i=visibleCases.findIndex(x=>x.review_case_id===currentId);if(visibleCases[i+n])loadDetail(visibleCases[i+n].review_case_id)}
async function saveNext(){document.querySelector('#state').textContent='Saving…';const old=currentId,r=await fetch(`/api/cases/${encodeURIComponent(old)}/verdict`,{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({human_verdict:document.querySelector('#verdict').value,final_classification:document.querySelector('#final').value,human_comment:document.querySelector('#comment').value,recommended_change_target:document.querySelector('#target').value,lesson_summary:''})});if(!r.ok){document.querySelector('#state').textContent='Save failed';return}await loadCases();const next=visibleCases.find(x=>x.review_case_id!==old);if(next)loadDetail(next.review_case_id)}
function chatHtml(items){return items.map(x=>`<div class="bubble ${x.role==='user'?'user':'assistant'}"><strong>${x.role==='user'?'Human':'Reviewer'}</strong><br>${esc(x.content)}</div>`).join('')}
function normalizeClass(value){return ['informational','promotion'].includes(value)?'ignored':value}function displayClass(value){if(!value)return '-';const normalized=normalizeClass(value);if(normalized!==value)return `${value} (legacy taxonomy → ${normalized})`;return classes.includes(value)?value:`${value} (legacy taxonomy)`}
async function askReviewer(){const panel=document.querySelector('#reviewerChat');panel.hidden=false;if(!document.querySelector('#chatLog').children.length)await sendReviewer('Please help me evaluate this case and the current human concern.')}
async function sendReviewer(initial){const input=document.querySelector('#chatInput'),message=initial||input.value.trim();if(!message)return;const log=document.querySelector('#chatLog');log.insertAdjacentHTML('beforeend',chatHtml([{role:'user',content:message}]));input.value='';const response=await fetch(`/api/cases/${encodeURIComponent(currentId)}/chat`,{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({message,human_comment:document.querySelector('#comment').value,human_verdict:document.querySelector('#verdict').value,final_classification:document.querySelector('#final').value,recommended_change_target:document.querySelector('#target').value})});if(!response.ok){log.insertAdjacentHTML('beforeend',chatHtml([{role:'assistant',content:'Reviewer request failed.'}]));return}const data=await response.json();log.insertAdjacentHTML('beforeend',chatHtml([{role:'assistant',content:data.reply}]));log.scrollTop=log.scrollHeight}
document.onkeydown=e=>{if((e.ctrlKey||e.metaKey)&&e.key==='Enter')saveNext();else if(e.altKey&&e.key==='ArrowRight')navigate(1);else if(e.altKey&&e.key==='ArrowLeft')navigate(-1)};function esc(v){return String(v??'').replaceAll('&','&amp;').replaceAll('<','&lt;').replaceAll('>','&gt;').replaceAll('"','&quot;').replaceAll("'",'&#039;')}initialize();
</script></body></html>"""
