from __future__ import annotations

import json
from http.server import BaseHTTPRequestHandler, HTTPServer
from urllib.parse import parse_qs, urlparse

from .review_models import HumanVerdictInput
from .review_service import ClassificationReviewService


def serve_review_ui(
    service: ClassificationReviewService,
    *,
    host: str = "127.0.0.1",
    port: int = 8791,
) -> None:
    class Handler(BaseHTTPRequestHandler):
        def do_GET(self) -> None:  # noqa: N802
            parsed = urlparse(self.path)
            if parsed.path == "/":
                self._html(_INDEX_HTML)
                return
            if parsed.path == "/api/cases":
                query = parse_qs(parsed.query)
                queue_only = query.get("queue_only", ["1"])[0] != "0"
                limit = int(query.get("limit", ["100"])[0])
                payload = [
                    _jsonable_case(item)
                    for item in service.list_cases(
                        queue_only=queue_only,
                        limit=limit,
                    )
                ]
                self._json(200, payload)
                return
            if parsed.path.startswith("/api/cases/"):
                review_case_id = parsed.path.removeprefix("/api/cases/")
                self._json(200, service.get_case_detail(review_case_id))
                return
            self._json(404, {"error": "not found"})

        def do_POST(self) -> None:  # noqa: N802
            parsed = urlparse(self.path)
            length = int(self.headers.get("Content-Length", "0"))
            if length > 1024 * 1024:
                self._json(413, {"error": "request too large"})
                return
            payload = json.loads(self.rfile.read(length) or b"{}")
            if parsed.path.endswith("/chat"):
                review_case_id = parsed.path.removeprefix("/api/cases/").removesuffix("/chat")
                reply = service.send_chat_message(
                    review_case_id,
                    str(payload.get("message") or ""),
                )
                self._json(200, {"reply": reply})
                return
            if parsed.path.endswith("/verdict"):
                review_case_id = parsed.path.removeprefix("/api/cases/").removesuffix("/verdict")
                record = service.save_verdict(
                    review_case_id,
                    HumanVerdictInput(
                        human_verdict=str(payload.get("human_verdict")),
                        final_classification=str(payload.get("final_classification") or ""),
                        lesson_summary=str(payload.get("lesson_summary") or ""),
                        recommended_change_target=str(
                            payload.get("recommended_change_target") or "none"
                        ),
                    ),
                )
                self._json(200, _jsonable_case(record))
                return
            self._json(404, {"error": "not found"})

        def log_message(self, format: str, *args: object) -> None:
            del format, args

        def _json(self, status: int, payload: object) -> None:
            body = json.dumps(payload, ensure_ascii=False).encode("utf-8")
            self.send_response(status)
            self.send_header("Content-Type", "application/json; charset=utf-8")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

        def _html(self, body: str) -> None:
            encoded = body.encode("utf-8")
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


_INDEX_HTML = """<!doctype html>
<html lang="en">
<meta charset="utf-8">
<title>Classification Review</title>
<style>
:root { --bg:#f6f3ea; --panel:#fffdf7; --ink:#1f1d1a; --muted:#6c655d; --line:#d9cfbf; --accent:#a24d2f; }
body { margin:0; font-family: ui-monospace, SFMono-Regular, Menlo, monospace; background:linear-gradient(135deg,#efe5d1,#f8f6ef); color:var(--ink); }
.app { display:grid; grid-template-columns: 360px 1fr; min-height:100vh; }
.sidebar,.detail { padding:16px; }
.sidebar { border-right:1px solid var(--line); background:rgba(255,253,247,.78); backdrop-filter: blur(4px); }
.detail { display:flex; flex-direction:column; gap:12px; }
.card { background:var(--panel); border:1px solid var(--line); border-radius:12px; padding:12px; }
.case { padding:10px; border:1px solid var(--line); border-radius:10px; margin-bottom:8px; cursor:pointer; background:#fff; }
.case.active { border-color:var(--accent); box-shadow:0 0 0 1px var(--accent) inset; }
.muted { color:var(--muted); font-size:12px; }
pre { white-space:pre-wrap; overflow-wrap:anywhere; background:#faf5ec; padding:10px; border-radius:10px; }
textarea,input,select,button { width:100%; box-sizing:border-box; font:inherit; padding:10px; border-radius:10px; border:1px solid var(--line); background:#fff; }
button { background:var(--accent); color:#fff; border:none; cursor:pointer; }
.row { display:grid; grid-template-columns:1fr 1fr; gap:12px; }
.chat { max-height:280px; overflow:auto; display:flex; flex-direction:column; gap:8px; }
.bubble { padding:10px; border-radius:10px; }
.bubble.user { background:#efe3d4; }
.bubble.assistant { background:#f9f6ef; border:1px solid var(--line); }
</style>
<body>
<div class="app">
  <aside class="sidebar">
    <div class="card">
      <label><input id="queueOnly" type="checkbox" checked> disagreement / uncertain only</label>
    </div>
    <div id="cases"></div>
  </aside>
  <main class="detail">
    <div id="detail" class="card">Select a review case.</div>
  </main>
</div>
<script>
const casesEl = document.getElementById("cases");
const detailEl = document.getElementById("detail");
const queueOnlyEl = document.getElementById("queueOnly");
let currentId = null;

queueOnlyEl.addEventListener("change", loadCases);

async function loadCases() {
  const queueOnly = queueOnlyEl.checked ? "1" : "0";
  const res = await fetch(`/api/cases?queue_only=${queueOnly}&limit=200`);
  const cases = await res.json();
  casesEl.innerHTML = "";
  for (const item of cases) {
    const div = document.createElement("div");
    div.className = "case" + (item.review_case_id === currentId ? " active" : "");
    div.innerHTML = `<strong>${item.review_case_id}</strong><br>${item.final_classification || "-"} → ${item.review_status}<div class="muted">${item.provider} ${item.message_id}</div>`;
    div.onclick = () => loadDetail(item.review_case_id);
    casesEl.appendChild(div);
  }
}

async function loadDetail(reviewCaseId) {
  currentId = reviewCaseId;
  await loadCases();
  const res = await fetch(`/api/cases/${reviewCaseId}`);
  const data = await res.json();
  const chat = (data.chat_history || []).map(item =>
    `<div class="bubble ${item.role}"><strong>${item.role}</strong><br>${escapeHtml(item.content)}</div>`
  ).join("");
  const context = data.context;
  detailEl.innerHTML = `
    <div class="row">
      <div class="card"><strong>${data.case.review_case_id}</strong><div class="muted">${context.provider} ${context.message_id}</div><div>Status: ${data.case.review_status}</div><div>Reason: ${escapeHtml(data.case.reason_summary)}</div></div>
      <div class="card"><div>Original: ${context.original_classification || "-"}</div><div>Final: ${context.final_classification || "-"}</div><div>Suggested: ${data.case.suggested_classification || "-"}</div></div>
    </div>
    <div class="row">
      <div class="card"><strong>Analysis Text</strong><pre>${escapeHtml(context.analysis_text || "")}</pre></div>
      <div class="card"><strong>System Decision</strong><pre>${escapeHtml(JSON.stringify({
        is_important: context.is_important,
        should_notify_user: context.should_notify_user,
        llm_should_notify_user: context.llm_should_notify_user,
        calendar_candidate: context.calendar_candidate,
        validator_result: context.validator_result,
        deterministic_corrections: context.deterministic_corrections,
        line_notification: context.line_notification,
        approval: context.approval,
        calendar_execution: context.calendar_execution
      }, null, 2))}</pre></div>
    </div>
    <div class="card"><strong>Reviewer Chat</strong><div class="chat" id="chat">${chat}</div><div class="row"><textarea id="chatInput" rows="3" placeholder="Ask about this case"></textarea><button onclick="sendChat('${reviewCaseId}')">Send</button></div></div>
    <div class="card">
      <strong>Save Verdict</strong>
      <div class="row">
        <select id="humanVerdict">
          <option value="original_correct">original_correct</option>
          <option value="reviewer_correct">reviewer_correct</option>
          <option value="modified">modified</option>
          <option value="unresolved">unresolved</option>
        </select>
        <input id="finalClassification" value="${escapeAttr(context.final_classification || "")}" placeholder="final_classification">
      </div>
      <div class="row">
        <select id="changeTarget">
          <option value="none">none</option>
          <option value="qwen_prompt">qwen_prompt</option>
          <option value="validator">validator</option>
          <option value="deterministic_rule">deterministic_rule</option>
          <option value="test">test</option>
        </select>
        <button onclick="saveVerdict('${reviewCaseId}')">Save Verdict</button>
      </div>
      <textarea id="lessonSummary" rows="4" placeholder="lesson_summary">${escapeHtml(data.case.lesson_summary || "")}</textarea>
    </div>
  `;
}

async function sendChat(reviewCaseId) {
  const message = document.getElementById("chatInput").value;
  await fetch(`/api/cases/${reviewCaseId}/chat`, {
    method:"POST",
    headers:{"Content-Type":"application/json"},
    body: JSON.stringify({message})
  });
  await loadDetail(reviewCaseId);
}

async function saveVerdict(reviewCaseId) {
  await fetch(`/api/cases/${reviewCaseId}/verdict`, {
    method:"POST",
    headers:{"Content-Type":"application/json"},
    body: JSON.stringify({
      human_verdict: document.getElementById("humanVerdict").value,
      final_classification: document.getElementById("finalClassification").value,
      recommended_change_target: document.getElementById("changeTarget").value,
      lesson_summary: document.getElementById("lessonSummary").value
    })
  });
  await loadDetail(reviewCaseId);
}

function escapeHtml(value) {
  return String(value).replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;");
}
function escapeAttr(value) {
  return escapeHtml(value).replaceAll('"', "&quot;");
}
loadCases();
</script>
</body>
</html>
"""
