from __future__ import annotations

import json
import os

from openai import OpenAI


def structured_response(
    client: OpenAI,
    *,
    schema: dict,
    schema_name: str,
    instructions: str,
    input_text: str,
    model: str | None = None,
) -> dict:
    response = client.responses.create(
        model=model or os.getenv("OPENAI_MODEL", "gpt-5.6"),
        instructions=instructions,
        input=input_text,
        text={
            "format": {
                "type": "json_schema",
                "name": schema_name,
                "strict": True,
                "schema": schema,
            }
        },
    )
    return json.loads(response.output_text)
