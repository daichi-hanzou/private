from __future__ import annotations

import html
import json
from dataclasses import asdict
from datetime import datetime, timezone
from pathlib import Path

from .cases import decision_case, related_case
from .display import DisplayIds, assign_display_ids
from .models import IngestionResult, NormalizedAuditEvent
from .normalizer import humanize, normalize_outcome
from .sequence import SequenceView, build_sequence


def _json_for_html(value: object) -> str:
    return json.dumps(value, ensure_ascii=False).replace("<", "\\u003c")


def _event_payload(
    event: NormalizedAuditEvent,
    ids: DisplayIds,
) -> dict:
    decision_key = event.decision_id or (
        event.correlation_id
        if event.event_type == "decision_made"
        else None
    )
    event_type_label = humanize(event.event_type)
    if event.event_type == "action_executed":
        event_type_label = {
            "propose_trade": "Proposal",
            "counteroffer_trade": "Counteroffer",
            "accept_trade": "Accept",
            "reject_trade": "Reject",
        }.get(event.action_type or "", "Action")
    display_status = event.status
    if event.event_type == "action_executed" and event.case_id:
        display_status = {
            "propose_trade": "Pending",
            "counteroffer_trade": "Countered",
            "accept_trade": "Accepted",
            "reject_trade": "Rejected",
        }.get(event.action_type or "", display_status)
    return {
        "display_id": ids.events[event.event_id],
        "event_id": event.event_id,
        "event_type": event.event_type,
        "event_type_label": event_type_label,
        "run_id": event.run_id,
        "day": event.day,
        "timestamp": event.timestamp.isoformat() if event.timestamp else None,
        "agent": event.actor_id,
        "agent_name": event.actor_name or humanize(event.actor_id),
        "counterparty": event.target_id,
        "action_type": event.action_type,
        "action_label": humanize(event.action_type),
        "decision_id": event.decision_id,
        "decision_display": ids.decisions.get(decision_key),
        "action_id": event.action_id,
        "action_display": ids.actions.get(event.action_id),
        "case_type": event.case_type,
        "case_id": event.case_id,
        "proposal_id": event.raw_event.get("proposal_id") or event.case_id,
        "case_display": ids.cases.get(event.case_id),
        "status": display_status,
        "summary": event.summary,
        "explanation": event.explanation,
        "expected": event.expected_outcome or "Not recorded",
        "actual": normalize_outcome(event.actual_outcome)
        or event.status
        or "Pending",
        "related": ids.related(event),
        "raw": event.raw_event,
    }


def _scope_data(
    events: list[NormalizedAuditEvent],
    ids: DisplayIds,
) -> tuple[dict[str, dict], dict[str, str]]:
    scopes: dict[str, dict] = {}
    event_scopes: dict[str, str] = {}
    for event in events:
        if event.case_id:
            key = f"case:{event.case_id}"
            scope_events = related_case(events, event.case_id)
        else:
            decision_key = event.decision_id or event.correlation_id
            if decision_key:
                key = f"decision:{decision_key}"
                scope_events = decision_case(events, decision_key)
            else:
                key = f"event:{event.event_id}"
                scope_events = [event]
        event_scopes[event.event_id] = key
        if key not in scopes:
            scopes[key] = {
                "title": (
                    f"Case {ids.cases.get(event.case_id, event.case_id)}"
                    if event.case_id
                    else f"Related to {ids.events[event.event_id]}"
                ),
                "event_ids": [item.event_id for item in scope_events],
                "business": build_sequence(
                    scope_events,
                    view="business",
                    display_ids=ids,
                ),
                "technical": build_sequence(
                    scope_events,
                    view="technical",
                    display_ids=ids,
                ),
            }
    return scopes, event_scopes


def _mapping(ids: DisplayIds) -> str:
    lines = [
        *(f"{value} = event_id: {key}" for key, value in ids.events.items()),
        *(
            f"{value} = decision_id: {key}"
            for key, value in ids.decisions.items()
        ),
        *(
            f"{value} = action_id: {key}"
            for key, value in ids.actions.items()
        ),
        *(f"{value} = case_id: {key}" for key, value in ids.cases.items()),
    ]
    return "\n".join(lines)


def render_explorer(
    events: list[NormalizedAuditEvent],
    *,
    ingestion: IngestionResult,
    view: SequenceView = "business",
    source_path: str | Path | None = None,
) -> str:
    ids = assign_display_ids(events)
    payload = [_event_payload(event, ids) for event in events]
    scopes, event_scopes = _scope_data(events, ids)
    options = {
        key: sorted({str(item.get(key) or "") for item in payload} - {""})
        for key in (
            "run_id",
            "day",
            "agent",
            "event_type",
            "action_type",
            "case_type",
            "case_id",
            "proposal_id",
            "decision_id",
            "status",
            "counterparty",
        )
    }
    issues = [
        {"line": issue.line, "message": issue.message}
        for issue in ingestion.issues
    ]
    data = {
        "events": payload,
        "scopes": scopes,
        "eventScopes": event_scopes,
        "options": options,
        "defaultView": view,
        "mapping": _mapping(ids),
        "ingestion": {
            "loaded": ingestion.loaded,
            "skipped": ingestion.skipped,
            "emptyLines": ingestion.empty_lines,
            "issues": issues,
        },
    }
    generated = datetime.now(timezone.utc).isoformat()
    source = html.escape(str(source_path or "audit_events.jsonl"))
    return f"""<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>AgentLedger Explorer</title>
<style>
:root{{--ink:#172a24;--paper:#f5efe4;--card:#fffdf8;--accent:#b74c2b;--line:#d8c9b4;--muted:#74685a;--table-header-height:38px}}
*{{box-sizing:border-box}} body{{margin:0;background:linear-gradient(145deg,#fbf8f0,var(--paper));color:var(--ink);font-family:Georgia,serif}}
header{{padding:28px clamp(18px,4vw,56px) 20px;border-bottom:1px solid var(--line)}} .eyebrow{{color:var(--accent);font:700 11px ui-monospace,monospace;letter-spacing:.18em;text-transform:uppercase}}
h1{{font-size:clamp(34px,6vw,68px);line-height:.95;margin:10px 0}} .sub{{color:var(--muted);font-family:ui-monospace,monospace;font-size:12px}}
input,select,button{{width:100%;border:1px solid var(--line);background:var(--card);padding:10px;color:var(--ink);font:12px ui-monospace,monospace}}
main{{padding:24px clamp(18px,4vw,56px) 60px}} .workspace{{display:grid;grid-template-columns:minmax(520px,1.35fr) minmax(330px,.65fr);gap:16px}}
.panel{{background:var(--card);border:1px solid var(--line);box-shadow:0 14px 36px #5c402315}} .panel h2{{font-size:18px;margin:0;padding:16px;border-bottom:1px solid var(--line)}}
.event-toolbar{{display:grid;grid-template-columns:minmax(220px,1fr) auto;gap:8px;padding:10px 12px;border-bottom:1px solid var(--line);background:#faf5ec}}
.event-toolbar button{{width:auto;white-space:nowrap}} .table-wrap{{overflow:auto;max-height:620px}} table{{width:100%;min-width:900px;border-collapse:separate;border-spacing:0;font-size:13px}}
.column-labels th{{position:sticky;top:0;z-index:4;height:var(--table-header-height);background:#efe5d5;text-align:left;font:700 10px ui-monospace,monospace;text-transform:uppercase;letter-spacing:.08em}}
.column-filters th{{position:sticky;top:var(--table-header-height);z-index:3;background:#f7f1e8;padding:6px}}
.column-filters th.active-filter{{background:#f8dfb7}} .column-filters input,.column-filters select{{min-width:90px;width:100%;padding:6px;font-size:10px}}
.column-filters th.active-filter input,.column-filters th.active-filter select{{border-color:var(--accent);box-shadow:inset 0 -2px 0 var(--accent)}}
.summary-column{{min-width:280px}}
th,td{{padding:11px;border-bottom:1px solid #e8ddce;vertical-align:top}} tbody tr{{cursor:pointer}} tbody tr:hover,tbody tr.active{{background:#f8dfb7}} .id{{color:var(--accent);font-weight:700;font-family:ui-monospace,monospace}}
.detail{{padding:18px;min-height:420px}} .detail-grid{{display:grid;grid-template-columns:1fr 1fr;gap:10px}} .field{{padding:10px;background:#f7f1e8}} .field span{{display:block;color:var(--muted);font:10px ui-monospace,monospace;text-transform:uppercase;margin-bottom:5px}}
.wide{{grid-column:1/-1}} pre{{white-space:pre-wrap;overflow-wrap:anywhere;background:#172a24;color:#f9eddb;padding:14px;font:11px/1.5 ui-monospace,monospace;max-height:360px;overflow:auto}}
.case,.sequence{{margin-top:16px}} .timeline{{padding:12px 18px}} .timeline-item{{border-left:3px solid var(--accent);padding:8px 12px;margin:6px 0;background:#faf3e8}}
.sequence-head{{display:flex;align-items:center;justify-content:space-between;padding:14px 16px;border-bottom:1px solid var(--line)}} .sequence-head h2{{border:0;padding:0}} .view-buttons{{display:flex;gap:6px}} .view-buttons button{{width:auto}} .view-buttons button.active{{background:var(--ink);color:white}}
.diagram{{overflow:auto;padding:20px;min-height:180px}} details{{margin-top:12px}} summary{{cursor:pointer;color:var(--accent);font-weight:700}}
.notice{{padding:10px 16px;background:#fff1c9;border:1px solid var(--line);margin-bottom:16px;font:12px ui-monospace,monospace}} .empty-state{{padding:28px;color:var(--muted);text-align:center}}
@media(max-width:980px){{.workspace{{grid-template-columns:1fr}}.event-toolbar{{grid-template-columns:1fr}}.event-toolbar button{{width:100%}}}}
</style></head>
<body><header><div class="eyebrow">Portable audit explorer / Phase 1</div><h1>AgentLedger</h1>
<div class="sub">Source: {source} · Generated: {generated}</div></header>
<main><div id="ingestion" class="notice"></div><section class="workspace">
<article class="panel event-list-panel"><h2>Event List <span id="count"></span></h2>
<div class="event-toolbar"><input id="search" type="search" placeholder="Search all event fields...">
<button id="clear-filters" type="button">Clear filters</button></div>
<div class="table-wrap"><table><thead>
<tr class="column-labels"><th>Day / Time</th><th>Event</th><th>Agent</th><th>Type</th><th class="summary-column">Summary</th><th>Related</th><th>Status</th></tr>
<tr class="column-filters">
<th><select data-column-filter="day"><option value="">All days</option></select></th>
<th><input type="search" data-column-filter="event" placeholder="ID..."></th>
<th><select data-column-filter="agent"><option value="">All agents</option></select></th>
<th><select data-column-filter="event_type"><option value="">All types</option></select></th>
<th><input type="search" data-column-filter="summary" placeholder="Text..."></th>
<th><input type="search" data-column-filter="related" placeholder="D1 / P1..."></th>
<th><select data-column-filter="status"><option value="">All statuses</option></select></th>
</tr></thead><tbody id="rows"></tbody></table></div></article>
<aside class="panel"><h2>Event Detail</h2><div id="detail" class="detail"></div></aside></section>
<section class="panel case"><h2>Related Case / Decision</h2><div id="timeline" class="timeline"></div></section>
<section class="panel sequence"><div class="sequence-head"><h2>Partial Sequence View</h2>
<div class="view-buttons"><button data-view="business">Business</button><button data-view="technical">Technical</button></div></div>
<div id="diagram" class="diagram"></div></section>
<details><summary>Audit ID mapping</summary><pre id="mapping"></pre></details>
<details><summary>Ingestion details</summary><pre id="issues"></pre></details></main>
<script>window.AGENT_LEDGER={_json_for_html(data)};</script>
<script type="module">
import mermaid from "https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs";
mermaid.initialize({{startOnLoad:false,theme:"base",securityLevel:"strict"}});
const data=window.AGENT_LEDGER; let visible=[...data.events], selected=null, view=data.defaultView;
const state={{globalSearch:"",columnFilters:{{day:"",event:"",agent:"",event_type:"",summary:"",related:"",status:""}}}};
const esc=v=>String(v??"").replace(/[&<>"']/g,c=>({{"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;","'":"&#39;"}}[c]));
const controls=[...document.querySelectorAll("[data-column-filter]")];
const uniqueSortedValues=(events,getter,compare=(a,b)=>String(a).localeCompare(String(b)))=>[...new Set(events.map(getter).filter(value=>value!==null&&value!==undefined&&value!==""))].sort(compare);
function setOptions(key,values,labelFor=value=>value){{const select=document.querySelector(`select[data-column-filter="${{key}}"]`);const initial=select.options[0].outerHTML;select.innerHTML=initial+values.map(value=>`<option value="${{esc(value)}}">${{esc(labelFor(value))}}</option>`).join("");}}
setOptions("day",uniqueSortedValues(data.events,event=>event.day,(a,b)=>Number(a)-Number(b)));
const agentNames=new Map(data.events.filter(event=>event.agent).map(event=>[event.agent,event.agent_name||event.agent]));
setOptions("agent",[...agentNames.keys()].sort((a,b)=>agentNames.get(a).localeCompare(agentNames.get(b))),value=>agentNames.get(value));
const typeOptions=uniqueSortedValues(data.events.flatMap(event=>[event.event_type_label,event.action_type?event.action_label:null]),value=>value);
setOptions("event_type",typeOptions);
setOptions("status",uniqueSortedValues(data.events,event=>event.status));
document.getElementById("search").addEventListener("input",applyFilters);
controls.forEach(control=>control.addEventListener(control.tagName==="SELECT"?"change":"input",applyFilters));
document.getElementById("clear-filters").addEventListener("click",()=>{{document.getElementById("search").value="";controls.forEach(control=>control.value="");applyFilters();}});
const contains=(value,query)=>String(value??"").toLowerCase().includes(query.toLowerCase());
function matchesEvent(event,key,value){{if(!value)return true;if(key==="day")return String(event.day??"")===value;if(key==="event")return contains(`${{event.display_id}} ${{event.event_id}}`,value);if(key==="agent")return event.agent===value;if(key==="event_type")return [event.event_type,event.event_type_label,event.action_type,event.action_label].some(item=>String(item??"").toLowerCase()===value.toLowerCase());if(key==="summary")return contains(`${{event.summary??""}} ${{event.explanation??""}}`,value);if(key==="related")return contains(`${{event.related.join(" ")}} ${{event.decision_display??""}} ${{event.case_display??""}} ${{event.proposal_id??""}} ${{event.case_id??""}}`,value);if(key==="status")return String(event.status??"")===value;return true;}}
function applyFilters(){{state.globalSearch=document.getElementById("search").value;controls.forEach(control=>{{state.columnFilters[control.dataset.columnFilter]=control.value;control.closest("th")?.classList.toggle("active-filter",Boolean(control.value));}});const globalQuery=state.globalSearch.toLowerCase();visible=data.events.filter(event=>{{if(globalQuery&&!JSON.stringify(event).toLowerCase().includes(globalQuery))return false;return Object.entries(state.columnFilters).every(([key,value])=>matchesEvent(event,key,value));}});const selectionVisible=selected&&visible.some(event=>event.event_id===selected.event_id);renderRows();if(!selectionVisible){{if(visible.length)selectEvent(visible[0].event_id);else clearSelection();}}}}
function renderRows(){{document.getElementById("count").textContent=`${{visible.length}} / ${{data.events.length}}`;document.getElementById("rows").innerHTML=visible.map(e=>`<tr data-id="${{esc(e.event_id)}}" class="${{selected?.event_id===e.event_id?"active":""}}"><td>${{esc(e.day??"-")}}<br><small>${{esc(e.timestamp?.slice(11,19)||"")}}</small></td><td class="id">${{esc(e.display_id)}}</td><td>${{esc(e.agent_name)}}</td><td>${{esc(e.event_type_label)}}</td><td class="summary-column">${{esc(e.summary)}}</td><td>${{esc(e.related.join(", "))}}</td><td>${{esc(e.status||"-")}}</td></tr>`).join("");document.querySelectorAll("#rows tr").forEach(row=>row.onclick=()=>selectEvent(row.dataset.id));}}
async function selectEvent(id){{selected=data.events.find(e=>e.event_id===id);renderRows();document.getElementById("detail").innerHTML=`<div class="detail-grid"><div class="field"><span>Event</span><b>${{esc(selected.display_id)}}</b></div><div class="field"><span>Type</span>${{esc(selected.event_type_label)}}</div><div class="field"><span>Agent / Day</span>${{esc(selected.agent_name)}} / ${{esc(selected.day??"-")}}</div><div class="field"><span>Related</span>${{esc(selected.related.join(", ")||"-")}}</div><div class="field wide"><span>Summary</span>${{esc(selected.summary)}}</div><div class="field wide"><span>Explanation</span>${{esc(selected.explanation||"Not recorded")}}</div><div class="field"><span>Expected</span>${{esc(selected.expected)}}</div><div class="field"><span>Actual</span>${{esc(selected.actual)}}</div></div><details><summary>View raw event JSON</summary><pre>${{esc(JSON.stringify(selected.raw,null,2))}}</pre></details>`;renderScope();}}
async function renderScope(){{const key=data.eventScopes[selected.event_id],scope=data.scopes[key];document.getElementById("timeline").innerHTML=`<b>${{esc(scope.title)}}</b>`+scope.event_ids.map(id=>{{const e=data.events.find(x=>x.event_id===id);return `<div class="timeline-item"><span class="id">${{esc(e.display_id)}}</span> · Day ${{esc(e.day??"-")}} · ${{esc(e.summary)}}</div>`}}).join("");document.querySelectorAll("[data-view]").forEach(b=>b.classList.toggle("active",b.dataset.view===view));const target=document.getElementById("diagram");target.innerHTML="";const node=document.createElement("div");node.className="mermaid";node.textContent=scope[view];target.appendChild(node);await mermaid.run({{nodes:[node]}});}}
function clearSelection(){{selected=null;document.getElementById("detail").innerHTML='<div class="empty-state">No event matches the current filters.</div>';document.getElementById("timeline").innerHTML='<div class="empty-state">No related events to display.</div>';document.getElementById("diagram").innerHTML='<div class="empty-state">No sequence to display.</div>';}}
document.querySelectorAll("[data-view]").forEach(b=>b.onclick=()=>{{view=b.dataset.view;if(selected)renderScope();}});
document.getElementById("mapping").textContent=data.mapping;document.getElementById("issues").textContent=JSON.stringify(data.ingestion.issues,null,2);
document.getElementById("ingestion").textContent=`Loaded: ${{data.ingestion.loaded}} · Skipped: ${{data.ingestion.skipped}} · Empty lines: ${{data.ingestion.emptyLines}}`;
applyFilters();
</script></body></html>"""


def write_explorer(
    output: str | Path,
    events: list[NormalizedAuditEvent],
    *,
    ingestion: IngestionResult,
    view: SequenceView = "business",
    source_path: str | Path | None = None,
) -> Path:
    path = Path(output)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        render_explorer(
            events,
            ingestion=ingestion,
            view=view,
            source_path=source_path,
        ),
        encoding="utf-8",
    )
    return path
