BUSINESS_PLAN_SCHEMA = {
    "type": "object",
    "additionalProperties": False,
    "required": [
        "company_name", "target_revenue_growth", "business_model_summary",
        "financial_summary", "key_growth_drivers", "growth_plan",
        "risk_assessment", "feasibility_assessment", "sources",
    ],
    "properties": {
        "company_name": {"type": "string"},
        "target_revenue_growth": {"type": "number"},
        "business_model_summary": {"type": "string"},
        "financial_summary": {"type": "string"},
        "key_growth_drivers": {"type": "array", "items": {"type": "string"}},
        "growth_plan": {
            "type": "array", "minItems": 3, "maxItems": 3,
            "items": {
                "type": "object", "additionalProperties": False,
                "required": [
                    "name", "description", "expected_revenue_impact",
                    "expected_profit_impact", "required_investment",
                    "implementation_difficulty", "main_risks", "evidence_source_ids",
                ],
                "properties": {
                    "name": {"type": "string"},
                    "description": {"type": "string"},
                    "expected_revenue_impact": {"type": ["string", "null"]},
                    "expected_profit_impact": {"type": ["string", "null"]},
                    "required_investment": {"type": ["string", "null"]},
                    "implementation_difficulty": {
                        "type": "string", "enum": ["High", "Medium", "Low"]
                    },
                    "main_risks": {"type": "array", "items": {"type": "string"}},
                    "evidence_source_ids": {
                        "type": "array", "items": {"type": "string"}
                    },
                },
            },
        },
        "risk_assessment": {"type": "array", "items": {"type": "string"}},
        "feasibility_assessment": {
            "type": "object", "additionalProperties": False,
            "required": ["rating", "rationale", "constraints", "evidence_source_ids"],
            "properties": {
                "rating": {"type": "string", "enum": ["High", "Medium", "Low"]},
                "rationale": {"type": "string"},
                "constraints": {"type": "array", "items": {"type": "string"}},
                "evidence_source_ids": {"type": "array", "items": {"type": "string"}},
            },
        },
        "sources": {
            "type": "array",
            "items": {
                "type": "object", "additionalProperties": False,
                "required": ["source_id", "file", "page", "category"],
                "properties": {
                    "source_id": {"type": "string"},
                    "file": {"type": "string"},
                    "page": {"type": ["integer", "null"]},
                    "category": {"type": "string"},
                },
            },
        },
    },
}
