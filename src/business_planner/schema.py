BUSINESS_PLAN_SCHEMA = {
    "type": "object",
    "additionalProperties": False,
    "required": [
        "company_name", "target_revenue_growth", "planning_period", "business_model_summary",
        "financial_summary", "key_growth_drivers", "growth_plan",
        "portfolio_decisions", "risk_assessment", "feasibility_assessment", "sources",
    ],
    "properties": {
        "company_name": {"type": "string"},
        "target_revenue_growth": {"type": "number"},
        "planning_period": {
            "type": "object", "additionalProperties": False,
            "required": ["base_fiscal_year", "target_fiscal_year", "horizon_years"],
            "properties": {
                "base_fiscal_year": {"type": ["integer", "null"]},
                "target_fiscal_year": {"type": ["integer", "null"]},
                "horizon_years": {"type": ["integer", "null"]},
            },
        },
        "business_model_summary": {"type": "string"},
        "financial_summary": {"type": "string"},
        "key_growth_drivers": {"type": "array", "items": {"type": "string"}},
        "growth_plan": {
            "type": "array", "minItems": 1, "maxItems": 8,
            "items": {
                "type": "object", "additionalProperties": False,
                "required": [
                    "name", "description", "expected_revenue_impact",
                    "expected_profit_impact", "required_investment",
                    "implementation_difficulty", "main_risks", "evidence_source_ids",
                    "portfolio_action",
                    "predecessor_initiative_names", "decision_rationale",
                    "resource_allocation",
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
                    "portfolio_action": {
                        "type": "string",
                        "enum": [
                            "Continue", "Expand", "Reduce", "Merge",
                            "Replace", "Terminate", "New",
                        ],
                    },
                    "predecessor_initiative_names": {
                        "type": "array", "items": {"type": "string"}
                    },
                    "decision_rationale": {"type": "string"},
                    "resource_allocation": {
                        "type": "object", "additionalProperties": False,
                        "required": [
                            "investment_million_yen", "headcount_fte",
                            "marketing_spend_million_yen",
                            "production_capacity_pct",
                            "allocation_rationale",
                        ],
                        "properties": {
                            "investment_million_yen": {"type": "number", "minimum": 0},
                            "headcount_fte": {"type": "integer", "minimum": 0},
                            "marketing_spend_million_yen": {
                                "type": "number", "minimum": 0
                            },
                            "production_capacity_pct": {
                                "type": "number", "minimum": 0, "maximum": 100
                            },
                            "allocation_rationale": {"type": "string"},
                        },
                    },
                },
            },
        },
        "portfolio_decisions": {
            "type": "array", "maxItems": 16,
            "items": {
                "type": "object", "additionalProperties": False,
                "required": [
                    "action", "predecessor_initiative_names",
                    "successor_initiative_names", "reason",
                    "investment_change_million_yen",
                    "headcount_change_fte",
                    "marketing_spend_change_million_yen",
                    "production_capacity_change_pct",
                ],
                "properties": {
                    "action": {
                        "type": "string",
                        "enum": [
                            "Continue", "Expand", "Reduce", "Merge",
                            "Replace", "Terminate", "New",
                        ],
                    },
                    "predecessor_initiative_names": {
                        "type": "array", "items": {"type": "string"}
                    },
                    "successor_initiative_names": {
                        "type": "array", "items": {"type": "string"}
                    },
                    "reason": {"type": "string"},
                    "investment_change_million_yen": {"type": "number"},
                    "headcount_change_fte": {"type": "integer"},
                    "marketing_spend_change_million_yen": {"type": "number"},
                    "production_capacity_change_pct": {"type": "number"},
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
