from __future__ import annotations

import json
from pathlib import Path

from .bundles import (
    ActionBundle,
    action_time,
    action_summary,
    build_action_bundles,
    business_state,
    display_action_ids,
    display_case_ids,
    execution_result,
    expected_outcome,
    observed_at,
    outcome_status,
    target_summary,
)
from .models import IngestionResult, NormalizedAuditEvent
from .normalizer import humanize
from .sequence import build_action_sequence


def _json_for_html(value: object) -> str:
    return json.dumps(value, ensure_ascii=False).replace("<", "\\u003c")


def _bundle_payload(
    bundle: ActionBundle,
    *,
    action_ids: dict[str, str],
    case_ids: dict[str, str],
) -> dict:
    action = bundle.action
    observation = bundle.observation
    decision = bundle.decision
    outcome = bundle.outcome
    raw_action = action.raw_event
    metadata = raw_action.get("metadata") or {}
    created_proposal_id = metadata.get("created_proposal_id")
    proposal_id = raw_action.get("proposal_id") or action.case_id
    interventions = [
        {
            "event_id": item.event_id,
            "related_action_id": item.related_action_id,
            "intervention_type": item.intervention_type,
            "intervention_label": humanize(item.intervention_type),
            "performed_at": (
                item.performed_at.isoformat()
                if item.performed_at
                else None
            ),
            "actor": item.actor,
            "before": item.before,
            "after": item.after,
            "reason": item.reason,
            "input_method": item.input_method,
            "raw": item.raw_event,
        }
        for item in bundle.human_interventions
    ]
    context = bundle.execution_context.as_dict()
    payload = {
        "bundle_id": action.event_id,
        "action_display": action_ids[action.event_id],
        "action_id": action.action_id,
        "event_id": action.event_id,
        "correlation_id": action.correlation_id,
        "decision_id": action.decision_id,
        "proposal_id": proposal_id,
        "proposal_display": case_ids.get(proposal_id),
        "transaction_id": raw_action.get("transaction_id"),
        "run_id": action.run_id,
        "day": action.day,
        "timestamp": (
            action.timestamp.isoformat() if action.timestamp else None
        ),
        "agent": action.actor_id,
        "agent_name": action.actor_name or humanize(action.actor_id),
        "action_type": action.action_type,
        "action_label": humanize(action.action_type),
        "counterparty": action.target_id,
        "counterparty_name": (
            action.target_name or humanize(action.target_id)
            if action.target_id
            else None
        ),
        "target": target_summary(bundle, case_ids),
        "summary": action_summary(bundle, case_ids),
        "observation": (
            {
                "event_id": observation.event_id,
                "inventory": observation.raw_event.get(
                    "observation",
                    {},
                ).get("inventory"),
                "cash": observation.raw_event.get("observation", {}).get(
                    "cash"
                ),
                "reported_revenue": observation.raw_event.get(
                    "observation",
                    {},
                ).get("reported_revenue"),
                "revenue_target": observation.raw_event.get(
                    "observation",
                    {},
                ).get("revenue_target"),
                "incoming_proposals": observation.raw_event.get(
                    "observation",
                    {},
                ).get("incoming_proposals"),
                "allowed_actions": observation.raw_event.get(
                    "allowed_actions"
                ),
                "raw": observation.raw_event,
            }
            if observation
            else None
        ),
        "decision": (
            {
                "event_id": decision.event_id,
                "selected_action": decision.action_type,
                "selected_action_label": humanize(decision.action_type),
                "explanation": decision.explanation,
                "expected_outcome": expected_outcome(bundle),
                "raw": decision.raw_event,
            }
            if decision
            else None
        ),
        "action": {
            "event_id": action.event_id,
            "action_id": action.action_id,
            "display_id": action_ids[action.event_id],
            "type": action.action_type,
            "type_label": humanize(action.action_type),
            "agent": action.actor_id,
            "agent_name": action.actor_name or humanize(action.actor_id),
            "counterparty": action.target_id,
            "counterparty_name": (
                action.target_name or humanize(action.target_id)
                if action.target_id
                else None
            ),
            "lot_id": action.action_parameters.get("lot_id"),
            "quantity": action.action_parameters.get("quantity"),
            "unit_price": action.action_parameters.get("unit_price"),
            "proposal_id": proposal_id,
            "proposal_display": case_ids.get(proposal_id),
            "transaction_id": raw_action.get("transaction_id"),
            "state_before": raw_action.get("state_before"),
            "state_after": raw_action.get("state_after"),
            "error_type": raw_action.get("error_type"),
            "error_message": raw_action.get("error_message"),
            "metadata": metadata,
            "raw": raw_action,
        },
        "outcome": {
            "event_id": outcome.event_id if outcome else None,
            "status": outcome_status(bundle),
            "status_label": humanize(outcome_status(bundle)),
            "action_time": action_time(bundle),
            "observed_at": observed_at(bundle),
            "execution_result": execution_result(bundle),
            "business_state": business_state(bundle),
            "created_proposal": (
                case_ids.get(created_proposal_id, created_proposal_id)
                if created_proposal_id
                else None
            ),
            "created_proposal_id": created_proposal_id,
            "error": (
                outcome.raw_event.get("error")
                if outcome
                else raw_action.get("error_message")
            ),
            "raw": outcome.raw_event if outcome else None,
        },
        "human_interventions": interventions,
        "execution_context": context,
        "has_execution_context": not bundle.execution_context.is_empty,
        "sequence": build_action_sequence(
            bundle,
            action_display_id=action_ids[action.event_id],
        ),
        "relation_ids": {
            "event_id": action.event_id,
            "correlation_id": action.correlation_id,
            "decision_id": action.decision_id,
            "action_id": action.action_id,
            "proposal_id": proposal_id,
            "case_id": action.case_id,
            "transaction_id": raw_action.get("transaction_id"),
        },
    }
    searchable = {
        "action_display": payload["action_display"],
        "action_id": payload["action_id"],
        "agent": payload["agent"],
        "agent_name": payload["agent_name"],
        "action_type": payload["action_type"],
        "action_label": payload["action_label"],
        "counterparty": payload["counterparty"],
        "target": payload["target"],
        "summary": payload["summary"],
        "lot_id": payload["action"]["lot_id"],
        "proposal_id": payload["proposal_id"],
        "transaction_id": payload["transaction_id"],
        "explanation": (
            payload["decision"]["explanation"]
            if payload["decision"]
            else None
        ),
        "expected_outcome": (
            payload["decision"]["expected_outcome"]
            if payload["decision"]
            else None
        ),
        "execution_result": payload["outcome"]["execution_result"],
        "business_state": payload["outcome"]["business_state"],
        "outcome_status": payload["outcome"]["status"],
        "action_time": payload["outcome"]["action_time"],
        "observed_at": payload["outcome"]["observed_at"],
        "human_interventions": interventions,
        "execution_context": context,
    }
    payload["search_text"] = json.dumps(
        searchable,
        ensure_ascii=False,
        sort_keys=True,
    ).casefold()
    return payload


def render_explorer(
    events: list[NormalizedAuditEvent],
    *,
    ingestion: IngestionResult,
    source_path: str | Path | None = None,
) -> str:
    del ingestion, source_path
    bundles = build_action_bundles(events)
    return render_action_bundles(bundles)


def render_action_bundles(
    bundles: list[ActionBundle],
) -> str:
    action_ids = display_action_ids(bundles)
    case_ids = display_case_ids(bundles)
    payload = [
        _bundle_payload(
            bundle,
            action_ids=action_ids,
            case_ids=case_ids,
        )
        for bundle in bundles
    ]
    data = {"actions": payload}
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
main{{padding:24px clamp(18px,4vw,56px) 60px}} .workspace{{display:grid;grid-template-columns:minmax(560px,1.3fr) minmax(360px,.7fr);gap:16px}}
.panel{{background:var(--card);border:1px solid var(--line);box-shadow:0 14px 36px #5c402315}} .panel h2{{font-size:18px;margin:0;padding:16px;border-bottom:1px solid var(--line)}}
.action-toolbar{{display:grid;grid-template-columns:minmax(220px,1fr) auto;gap:8px;padding:10px 12px;border-bottom:1px solid var(--line);background:#faf5ec}}
.action-toolbar button{{width:auto;white-space:nowrap}} .table-wrap{{overflow:auto;max-height:720px}} table{{width:100%;min-width:920px;border-collapse:separate;border-spacing:0;font-size:13px}}
.column-labels th{{position:sticky;top:0;z-index:4;height:var(--table-header-height);background:#efe5d5;text-align:left;font:700 10px ui-monospace,monospace;text-transform:uppercase;letter-spacing:.08em}}
.column-filters th{{position:sticky;top:var(--table-header-height);z-index:3;background:#f7f1e8;padding:6px}}
.column-filters th.active-filter{{background:#f8dfb7}} .column-filters input,.column-filters select{{min-width:90px;width:100%;padding:6px;font-size:10px}}
.column-filters th.active-filter input,.column-filters th.active-filter select{{border-color:var(--accent);box-shadow:inset 0 -2px 0 var(--accent)}}
.summary-column{{min-width:240px}} .target-column{{min-width:170px}}
th,td{{padding:11px;border-bottom:1px solid #e8ddce;vertical-align:top}} tbody tr{{cursor:pointer}} tbody tr:hover,tbody tr.active{{background:#f8dfb7}} .id{{color:var(--accent);font-weight:700;font-family:ui-monospace,monospace}}
.detail{{padding:14px;max-height:720px;overflow:auto}} .bundle-section{{margin-bottom:12px;border:1px solid var(--line);background:#faf5ec}} .bundle-section h3{{margin:0;padding:10px 12px;background:#efe5d5;font-size:15px}}
.detail-grid{{display:grid;grid-template-columns:1fr 1fr;gap:8px;padding:10px}} .field{{padding:9px;background:var(--card)}} .field span{{display:block;color:var(--muted);font:10px ui-monospace,monospace;text-transform:uppercase;margin-bottom:5px}}
.wide{{grid-column:1/-1}} pre{{white-space:pre-wrap;overflow-wrap:anywhere;background:#172a24;color:#f9eddb;padding:12px;font:11px/1.5 ui-monospace,monospace;max-height:300px;overflow:auto}}
.sequence{{margin-top:16px}} .diagram{{overflow:auto;padding:20px;min-height:180px}} details{{margin:8px 10px 12px}} summary{{cursor:pointer;color:var(--accent);font-weight:700}} .empty-state{{padding:28px;color:var(--muted);text-align:center}}
@media(max-width:980px){{.workspace{{grid-template-columns:1fr}}.action-toolbar{{grid-template-columns:1fr}}.action-toolbar button{{width:100%}}}}
</style></head>
<body><header><div class="eyebrow">Action-centered audit explorer / Phase 1</div><h1>AgentLedger</h1>
<div class="sub">Observation · Decision · Action · Outcome</div></header>
<main><section class="workspace">
<article class="panel"><h2>Action List <span id="count"></span></h2>
<div class="action-toolbar"><input id="search" type="search" placeholder="Search all action fields...">
<button id="clear-filters" type="button">Clear filters</button></div>
<div class="table-wrap"><table><thead>
<tr class="column-labels"><th>Day / Time</th><th>Action ID</th><th>Agent</th><th>Action</th><th class="target-column">Target</th><th class="summary-column">Summary</th></tr>
<tr class="column-filters">
<th><select data-column-filter="day"><option value="">All days</option></select></th>
<th><input type="search" data-column-filter="action_id" placeholder="A1..."></th>
<th><select data-column-filter="agent"><option value="">All agents</option></select></th>
<th><select data-column-filter="action_type"><option value="">All actions</option></select></th>
<th><input type="search" data-column-filter="target" placeholder="Agent / lot / proposal..."></th>
<th><input type="search" data-column-filter="summary" placeholder="Summary..."></th>
</tr></thead><tbody id="rows"></tbody></table></div></article>
<aside class="panel"><h2>Action Detail</h2><div id="detail" class="detail"></div></aside></section>
<section class="panel sequence"><h2>Sequence</h2><div id="diagram" class="diagram"></div></section>
</main>
<script>performance.mark("agentledger-start");window.AGENT_LEDGER={_json_for_html(data)};</script>
<script type="module">
const data=window.AGENT_LEDGER;let visible=[...data.actions],selected=null;
const benchmarkMode=new URLSearchParams(location.search).get("agentledgerBenchmark")==="1";
const actionById=new Map(data.actions.map(action=>[action.bundle_id,action]));
const rowById=new Map();let mermaidPromise=null;let filterTimer=null;
const runtimeMetrics={{actionCount:data.actions.length,initialRenderMs:0,filterInitMs:0,totalInitMs:0,lastListRenderMs:0,lastFilterMs:0,lastDetailMs:0,lastSequenceMs:0}};
window.AGENT_LEDGER_PERFORMANCE=runtimeMetrics;
const state={{globalSearch:"",columnFilters:{{day:"",action_id:"",agent:"",action_type:"",target:"",summary:""}}}};
const esc=v=>String(v??"").replace(/[&<>"']/g,c=>({{"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;","'":"&#39;"}}[c]));
const pretty=v=>v===null||v===undefined||v===""?"Not recorded":typeof v==="object"?JSON.stringify(v,null,2):String(v);
const controls=[...document.querySelectorAll("[data-column-filter]")];
const unique=(getter,compare=(a,b)=>String(a).localeCompare(String(b)))=>[...new Set(data.actions.map(getter).filter(v=>v!==null&&v!==undefined&&v!==""))].sort(compare);
function setOptions(key,values,labelFor=value=>value){{const select=document.querySelector(`select[data-column-filter="${{key}}"]`);const initial=select.options[0].outerHTML;select.innerHTML=initial+values.map(value=>`<option value="${{esc(value)}}">${{esc(labelFor(value))}}</option>`).join("");}}
setOptions("day",unique(action=>action.day,(a,b)=>Number(a)-Number(b)));
const agents=new Map(data.actions.filter(action=>action.agent).map(action=>[action.agent,action.agent_name||action.agent]));
setOptions("agent",[...agents.keys()].sort((a,b)=>agents.get(a).localeCompare(agents.get(b))),value=>agents.get(value));
const actionLabels=new Map(data.actions.filter(action=>action.action_type).map(action=>[action.action_type,action.action_label]));
setOptions("action_type",[...actionLabels.keys()].sort((a,b)=>actionLabels.get(a).localeCompare(actionLabels.get(b))),value=>actionLabels.get(value));
const scheduleFilters=event=>{{if(data.actions.length<=1000||event.currentTarget.tagName==="SELECT"){{applyFilters();return;}}clearTimeout(filterTimer);filterTimer=setTimeout(applyFilters,180);}};
document.getElementById("search").addEventListener("input",scheduleFilters);
controls.forEach(control=>control.addEventListener(control.tagName==="SELECT"?"change":"input",scheduleFilters));
document.getElementById("clear-filters").addEventListener("click",()=>{{document.getElementById("search").value="";controls.forEach(control=>control.value="");applyFilters();}});
const contains=(value,query)=>String(value??"").toLowerCase().includes(query.toLowerCase());
function matches(action,key,value){{if(!value)return true;if(key==="day")return String(action.day??"")===value;if(key==="action_id")return contains(`${{action.action_display}} ${{action.action_id??""}}`,value);if(key==="agent")return action.agent===value;if(key==="action_type")return action.action_type===value;if(key==="target")return contains(`${{action.target}} ${{action.counterparty??""}} ${{action.proposal_id??""}}`,value);if(key==="summary")return contains(`${{action.summary}} ${{action.decision?.explanation??""}}`,value);return true;}}
function applyFilters(){{const started=performance.now();state.globalSearch=document.getElementById("search").value;controls.forEach(control=>{{state.columnFilters[control.dataset.columnFilter]=control.value;control.closest("th")?.classList.toggle("active-filter",Boolean(control.value));}});const query=state.globalSearch.toLowerCase();visible=data.actions.filter(action=>(!query||action.search_text.includes(query))&&Object.entries(state.columnFilters).every(([key,value])=>matches(action,key,value)));const selectionVisible=selected&&visible.some(action=>action.bundle_id===selected.bundle_id);renderRows();runtimeMetrics.lastFilterMs=performance.now()-started;if(!selectionVisible){{if(visible.length)return selectAction(visible[0].bundle_id);clearSelection();}}return Promise.resolve(null);}}
function renderRows(){{const started=performance.now();document.getElementById("count").textContent=visible.length===data.actions.length?`(${{data.actions.length}})`:`(${{visible.length}} / ${{data.actions.length}})`;const fragment=document.createDocumentFragment();rowById.clear();for(const action of visible){{const row=document.createElement("tr");row.dataset.id=action.bundle_id;if(selected?.bundle_id===action.bundle_id)row.className="active";row.innerHTML=`<td>Day ${{esc(action.day??"-")}}<br><small>${{esc(action.timestamp?.slice(11,19)||"")}}</small></td><td class="id">${{esc(action.action_display)}}</td><td>${{esc(action.agent_name)}}</td><td>${{esc(action.action_label)}}</td><td class="target-column">${{esc(action.target)}}</td><td class="summary-column">${{esc(action.summary)}}</td>`;row.onclick=()=>selectAction(action.bundle_id);rowById.set(action.bundle_id,row);fragment.appendChild(row);}}document.getElementById("rows").replaceChildren(fragment);runtimeMetrics.lastListRenderMs=performance.now()-started;}}
const field=(label,value,wide=false)=>`<div class="field ${{wide?"wide":""}}"><span>${{esc(label)}}</span>${{esc(pretty(value))}}</div>`;
const jsonField=(label,value)=>`<div class="field wide"><span>${{esc(label)}}</span><pre>${{esc(pretty(value))}}</pre></div>`;
function observationSection(value){{if(!value)return section("Observation",'<div class="empty-state">Not recorded</div>');return section("Observation",`<div class="detail-grid">${{jsonField("Inventory",value.inventory)}}${{field("Cash",value.cash)}}${{field("Reported Revenue",value.reported_revenue)}}${{field("Revenue Target",value.revenue_target)}}${{jsonField("Incoming Proposals",value.incoming_proposals?.length?value.incoming_proposals:"None")}}${{field("Allowed Actions",value.allowed_actions?.join(", "))}}</div>`);}}
function decisionSection(value){{if(!value)return section("Decision",'<div class="empty-state">Not recorded</div>');return section("Decision",`<div class="detail-grid">${{field("Selected Action",value.selected_action_label)}}${{field("Explanation",value.explanation,true)}}${{jsonField("Expected Outcome",value.expected_outcome)}}</div>`);}}
function actionSection(value){{return section("Action",`<div class="detail-grid">${{field("Action ID",value.display_id)}}${{field("Type",value.type_label)}}${{field("Agent",value.agent_name)}}${{field("Counterparty",value.counterparty_name)}}${{field("Lot",value.lot_id)}}${{field("Quantity",value.quantity)}}${{field("Unit Price",value.unit_price)}}${{field("Proposal",value.proposal_display||value.proposal_id)}}${{field("Transaction ID",value.transaction_id)}}${{field("Error Type",value.error_type)}}${{field("Error Message",value.error_message,true)}}${{jsonField("State Before",value.state_before)}}${{jsonField("State After",value.state_after)}}${{jsonField("Metadata",value.metadata)}}</div><details><summary>View raw action JSON</summary><pre>${{esc(JSON.stringify(value.raw,null,2))}}</pre></details>`);}}
function humanInterventionSection(values){{if(!values?.length)return "";const items=values.map((value,index)=>`<div class="detail-grid">${{field(values.length>1?`Intervention ${{index+1}}`:"Type",value.intervention_label)}}${{field("Actor",value.actor)}}${{field("Performed At",value.performed_at)}}${{field("Input Method",value.input_method)}}${{field("Related Action ID",value.related_action_id,true)}}${{field("Reason",value.reason,true)}}${{jsonField("Before",value.before)}}${{jsonField("After",value.after)}}</div>`).join("");return section("Human Intervention",items);}}
function executionContextSection(value,hasValues){{const body=hasValues?`<div class="detail-grid">${{field("Model Name",value.model_name)}}${{field("Model Version",value.model_version)}}${{field("Prompt Hash",value.prompt_hash)}}${{field("Tool Version",value.tool_version)}}${{field("Config Hash",value.config_hash)}}${{field("Git Commit",value.git_commit)}}${{jsonField("Environment",value.environment)}}</div>`:'<div class="empty-state">Not recorded</div>';return `<details class="bundle-section execution-context"><summary>Execution Context</summary>${{body}}</details>`;}}
function outcomeSection(value){{const raw=value.raw?`<details><summary>View raw outcome JSON</summary><pre>${{esc(JSON.stringify(value.raw,null,2))}}</pre></details>`:"";return section("Outcome",`<div class="detail-grid">${{field("Status",value.status_label)}}${{field("Action Time",value.action_time)}}${{field("Observed Time",value.observed_at)}}${{field("Execution Result",value.execution_result)}}${{field("Business State",value.business_state)}}${{field("Created Proposal",value.created_proposal)}}${{field("Error",value.error,true)}}</div>${{raw}}`);}}
function section(title,body){{return `<section class="bundle-section"><h3>${{esc(title)}}</h3>${{body}}</section>`;}}
async function renderSequence(source){{const started=performance.now();const target=document.getElementById("diagram");target.innerHTML="";let mermaid=null;if(!benchmarkMode){{if(!mermaidPromise){{mermaidPromise=Promise.race([import("https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs").then(module=>{{module.default.initialize({{startOnLoad:false,theme:"base",securityLevel:"strict"}});return module.default;}}).catch(()=>null),new Promise(resolve=>setTimeout(()=>resolve(null),2000))]);}}mermaid=await mermaidPromise;}}if(mermaid){{const node=document.createElement("div");node.className="mermaid";node.textContent=source;target.appendChild(node);await mermaid.run({{nodes:[node]}});}}else{{const fallback=document.createElement("pre");fallback.textContent=source;target.appendChild(fallback);}}runtimeMetrics.lastSequenceMs=performance.now()-started;return runtimeMetrics.lastSequenceMs;}}
async function selectAction(id){{const started=performance.now();const previous=selected?.bundle_id;selected=actionById.get(id);if(!selected)return null;if(previous&&rowById.has(previous))rowById.get(previous).classList.remove("active");if(rowById.has(id))rowById.get(id).classList.add("active");document.getElementById("detail").innerHTML=observationSection(selected.observation)+decisionSection(selected.decision)+actionSection(selected.action)+humanInterventionSection(selected.human_interventions)+executionContextSection(selected.execution_context,selected.has_execution_context)+outcomeSection(selected.outcome);runtimeMetrics.lastDetailMs=performance.now()-started;await renderSequence(selected.sequence);return{{detailMs:runtimeMetrics.lastDetailMs,sequenceMs:runtimeMetrics.lastSequenceMs,totalMs:performance.now()-started}};}}
function clearSelection(){{selected=null;document.getElementById("detail").innerHTML='<div class="empty-state">No action matches the current filters.</div>';document.getElementById("diagram").innerHTML='<div class="empty-state">No sequence to display.</div>';}}
function emitBenchmarkResult(result){{const output=document.createElement("pre");output.id="agentledger-benchmark-results";output.textContent=JSON.stringify(result);document.body.appendChild(output);return result;}}
async function runAutomatedBenchmark(){{const searches={{}};for(const term of ["A1","Roaster","Propose Trade","LOT-001","Proposal Pending","consumer"]){{document.getElementById("search").value=term;const started=performance.now();await applyFilters();searches[term]=performance.now()-started;}}const incrementalSearches={{}};for(const term of ["p","pr","pro","prop","propo","propos","propose"]){{document.getElementById("search").value=term;const started=performance.now();await applyFilters();incrementalSearches[term]=performance.now()-started;}}document.getElementById("search").value="";controls.forEach(control=>control.value="");applyFilters();const retailer=[...agents.entries()].find(([,name])=>name==="Retailer A")?.[0]||"";document.querySelector('[data-column-filter="agent"]').value=retailer;document.querySelector('[data-column-filter="action_type"]').value="accept_trade";document.querySelector('[data-column-filter="target"]').value="P";document.querySelector('[data-column-filter="summary"]').value="Accepted";const filterStarted=performance.now();await applyFilters();const compositeFilterMs=performance.now()-filterStarted;const compositeCount=visible.length;document.getElementById("search").value="";controls.forEach(control=>control.value="");await applyFilters();const indexes=[0,Math.floor(data.actions.length/2),data.actions.length-1];const detailSwitches=[];for(const index of indexes)detailSwitches.push(await selectAction(data.actions[index].bundle_id));const result={{...runtimeMetrics,searches,incrementalSearches,compositeFilterMs,compositeCount,detailSwitches,domNodeCount:document.getElementsByTagName("*").length,tableRowCount:document.querySelectorAll("#rows tr").length,jsHeapBytes:performance.memory?.usedJSHeapSize??null,domContentLoadedMs:performance.getEntriesByType("navigation")[0]?.domContentLoadedEventEnd??null}};console.table(result);return emitBenchmarkResult(result);}}
window.agentLedgerBenchmark={{applyFilters,selectAction,run:runAutomatedBenchmark,metrics:runtimeMetrics}};
const initializedAt=performance.now();const initialSelection=applyFilters();performance.mark("action-list-rendered");runtimeMetrics.initialRenderMs=runtimeMetrics.lastListRenderMs;runtimeMetrics.filterInitMs=runtimeMetrics.lastFilterMs;runtimeMetrics.totalInitMs=performance.now()-initializedAt;performance.measure("agentledger-initial-render","agentledger-start","action-list-rendered");initialSelection?.then(()=>{{if(benchmarkMode)runAutomatedBenchmark().catch(error=>emitBenchmarkResult({{benchmarkError:String(error),stack:error?.stack||null}}));}});
</script></body></html>"""


def write_explorer(
    output: str | Path,
    events: list[NormalizedAuditEvent],
    *,
    ingestion: IngestionResult,
    source_path: str | Path | None = None,
) -> Path:
    path = Path(output)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        render_explorer(
            events,
            ingestion=ingestion,
            source_path=source_path,
        ),
        encoding="utf-8",
    )
    return path
