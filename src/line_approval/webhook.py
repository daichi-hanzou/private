from __future__ import annotations

import json
from http.server import BaseHTTPRequestHandler, HTTPServer

from .service import LineApprovalService


def serve_webhook(
    service: LineApprovalService, *, host: str = "127.0.0.1", port: int = 8787
) -> None:
    class Handler(BaseHTTPRequestHandler):
        def do_GET(self) -> None:  # noqa: N802
            if self.path != "/health":
                self._respond(404, {"error": "not found"})
                return
            self._respond(200, {"status": "ok"})

        def do_POST(self) -> None:  # noqa: N802
            if self.path != "/line/webhook":
                self._respond(404, {"error": "not found"})
                return
            try:
                length = int(self.headers.get("Content-Length", "0"))
            except ValueError:
                self._respond(400, {"error": "invalid content length"})
                return
            if length < 0 or length > 1024 * 1024:
                self._respond(413, {"error": "request too large"})
                return
            raw_body = self.rfile.read(length)
            result = service.handle_webhook(
                raw_body, self.headers.get("x-line-signature")
            )
            self._respond(result.status_code, {"status": result.message})

        def log_message(self, format: str, *args: object) -> None:
            del format, args

        def _respond(self, status: int, payload: dict[str, str]) -> None:
            body = json.dumps(payload).encode()
            self.send_response(status)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

    HTTPServer((host, port), Handler).serve_forever()
