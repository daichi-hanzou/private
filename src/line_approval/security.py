from __future__ import annotations

import base64
import hashlib
import hmac
from urllib.parse import parse_qs, urlencode


def verify_signature(raw_body: bytes, signature: str | None, secret: str) -> bool:
    if not signature or not secret:
        return False
    digest = hmac.new(secret.encode(), raw_body, hashlib.sha256).digest()
    expected = base64.b64encode(digest).decode("ascii")
    return hmac.compare_digest(expected, signature)


def token_hash(token: str) -> str:
    return hashlib.sha256(token.encode()).hexdigest()


def user_hash(user_id: str) -> str:
    return hashlib.sha256(user_id.encode()).hexdigest()


def masked_actor(user_id: str) -> str:
    return f"line:{user_hash(user_id)[:12]}"


def encode_postback(action: str, approval_id: str, token: str) -> str:
    if action not in {"approve", "reject"}:
        raise ValueError("unsupported LINE approval action")
    return urlencode({"action": action, "approval_id": approval_id, "token": token})


def decode_postback(value: str) -> tuple[str, str, str]:
    parsed = parse_qs(value, strict_parsing=True, keep_blank_values=False)
    if set(parsed) != {"action", "approval_id", "token"} or any(
        len(items) != 1 for items in parsed.values()
    ):
        raise ValueError("invalid LINE postback data")
    action, approval_id, token = (
        parsed["action"][0], parsed["approval_id"][0], parsed["token"][0]
    )
    if action not in {"approve", "reject"}:
        raise ValueError("invalid LINE postback action")
    if not approval_id.startswith("AP-") or not token:
        raise ValueError("invalid LINE approval reference")
    return action, approval_id, token
