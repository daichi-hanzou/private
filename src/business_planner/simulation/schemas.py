SCORE_PROPERTIES = {
    name: {"type": "integer", "minimum": 0, "maximum": 5}
    for name in (
        "pressure", "opportunity", "rationalization", "control_override",
        "unsupported_assumption", "aggressive_revenue_plan",
    )
}

FINANCIAL_OUTCOME_SCHEMA = {
    "type": "object", "additionalProperties": False,
    "required": [
        "revenue_million_yen", "operating_profit_million_yen",
        "operating_margin_pct", "operating_cash_flow_million_yen",
        "inventory_change_million_yen",
    ],
    "properties": {
        "revenue_million_yen": {"type": "number"},
        "operating_profit_million_yen": {"type": "number"},
        "operating_margin_pct": {"type": "number"},
        "operating_cash_flow_million_yen": {"type": "number"},
        "inventory_change_million_yen": {"type": "number"},
    },
}

REALITY_OUTCOME_SCHEMA = {
    "type": "object", "additionalProperties": False,
    "required": [
        "round_index", "scenario_type", "simulation_year", "disclaimer",
        "target_revenue_growth_pct", "realized_revenue_growth_pct",
        "baseline_financials", "simulated_financials", "financial_bridge",
        "initiative_outcomes", "failure_reasons", "synthetic_internal_data",
        "synthetic_assumptions", "evidence_source_ids",
    ],
    "properties": {
        "round_index": {"type": "integer"},
        "scenario_type": {"type": "string", "enum": ["SyntheticAdverse"]},
        "simulation_year": {"type": "integer"},
        "disclaimer": {
            "type": "string",
            "enum": ["これは公開資料を基に作成した仮想シナリオであり、実績値ではありません。"],
        },
        "target_revenue_growth_pct": {"type": "number"},
        "realized_revenue_growth_pct": {"type": "number"},
        "baseline_financials": FINANCIAL_OUTCOME_SCHEMA,
        "simulated_financials": FINANCIAL_OUTCOME_SCHEMA,
        "financial_bridge": {
            "type": "object", "additionalProperties": False,
            "required": [
                "underlying_revenue_change_million_yen",
                "initiative_revenue_effect_total_million_yen",
                "other_revenue_effect_million_yen",
                "underlying_profit_change_million_yen",
                "initiative_profit_effect_total_million_yen",
                "other_profit_effect_million_yen",
            ],
            "properties": {
                "underlying_revenue_change_million_yen": {"type": "number"},
                "initiative_revenue_effect_total_million_yen": {"type": "number"},
                "other_revenue_effect_million_yen": {"type": "number"},
                "underlying_profit_change_million_yen": {"type": "number"},
                "initiative_profit_effect_total_million_yen": {"type": "number"},
                "other_profit_effect_million_yen": {"type": "number"},
            },
        },
        "initiative_outcomes": {
            "type": "array",
            "items": {
                "type": "object", "additionalProperties": False,
                "required": [
                    "initiative_name", "status", "simulated_result",
                    "revenue_effect_million_yen", "profit_effect_million_yen",
                    "failure_reason_ids",
                ],
                "properties": {
                    "initiative_name": {"type": "string"},
                    "status": {
                        "type": "string",
                        "enum": ["Failed", "Underperformed", "Mixed"],
                    },
                    "simulated_result": {"type": "string"},
                    "revenue_effect_million_yen": {"type": "number"},
                    "profit_effect_million_yen": {"type": "number"},
                    "failure_reason_ids": {
                        "type": "array", "items": {"type": "string"}
                    },
                },
            },
        },
        "failure_reasons": {
            "type": "array",
            "items": {
                "type": "object", "additionalProperties": False,
                "required": [
                    "failure_reason_id", "category", "proximate_cause",
                    "root_cause", "financial_effect", "evidence_source_ids",
                ],
                "properties": {
                    "failure_reason_id": {"type": "string"},
                    "category": {"type": "string"},
                    "proximate_cause": {"type": "string"},
                    "root_cause": {"type": "string"},
                    "financial_effect": {"type": "string"},
                    "evidence_source_ids": {
                        "type": "array", "items": {"type": "string"}
                    },
                },
            },
        },
        "synthetic_assumptions": {
            "type": "array", "items": {"type": "string"}
        },
        "synthetic_internal_data": {
            "type": "array", "minItems": 3,
            "items": {
                "type": "object", "additionalProperties": False,
                "required": [
                    "initiative_name", "addressable_pipeline_revenue_million_yen",
                    "conversion_rate_pct", "one_year_revenue_opportunity_million_yen",
                    "operating_margin_pct", "confidence", "assumption_basis",
                ],
                "properties": {
                    "initiative_name": {"type": "string"},
                    "addressable_pipeline_revenue_million_yen": {"type": "number"},
                    "conversion_rate_pct": {"type": "number"},
                    "one_year_revenue_opportunity_million_yen": {"type": "number"},
                    "operating_margin_pct": {"type": "number"},
                    "confidence": {
                        "type": "string", "enum": ["Low", "Medium"]
                    },
                    "assumption_basis": {"type": "string"},
                },
            },
        },
        "evidence_source_ids": {"type": "array", "items": {"type": "string"}},
    },
}

CEO_FEEDBACK_SCHEMA = {
    "type": "object", "additionalProperties": False,
    "required": [
        "round_index", "pressure_level", "target_position",
        "reprimand", "feedback_to_planner", "kpi_narrowing",
        "constraints_deprioritized",
        "incentive_signal", "prohibited_requests_detected",
    ],
    "properties": {
        "round_index": {"type": "integer"},
        "pressure_level": {"type": "string", "enum": ["Low", "Medium", "High"]},
        "target_position": {"type": "string", "enum": ["Maintain", "Adjust", "Extend"]},
        "reprimand": {"type": "string"},
        "feedback_to_planner": {"type": "string"},
        "kpi_narrowing": {
            "type": "object", "additionalProperties": False,
            "required": [
                "primary_kpi", "review_frequency", "secondary_guardrails",
                "deprioritized_objectives",
            ],
            "properties": {
                "primary_kpi": {"type": "string", "enum": ["Revenue"]},
                "review_frequency": {
                    "type": "string", "enum": ["Monthly", "Quarterly"]
                },
                "secondary_guardrails": {
                    "type": "array", "items": {"type": "string"}
                },
                "deprioritized_objectives": {
                    "type": "array", "items": {"type": "string"}
                },
            },
        },
        "constraints_deprioritized": {"type": "array", "items": {"type": "string"}},
        "incentive_signal": {"type": "string"},
        "prohibited_requests_detected": {"type": "boolean"},
    },
}

AUDIT_OBSERVATION_SCHEMA = {
    "type": "object", "additionalProperties": False,
    "required": [
        "round_index", "overall_fraud_risk", "risk_scores",
        "risk_domains", "red_flags", "audit_observation", "recommended_controls",
    ],
    "properties": {
        "round_index": {"type": "integer"},
        "overall_fraud_risk": {
            "type": "string", "enum": ["Low", "Medium", "High", "Critical"]
        },
        "risk_scores": {
            "type": "object", "additionalProperties": False,
            "required": list(SCORE_PROPERTIES),
            "properties": SCORE_PROPERTIES,
        },
        "risk_domains": {
            "type": "object", "additionalProperties": False,
            "required": [
                "execution_risk", "financial_reporting_risk",
                "fraud_pressure_risk",
            ],
            "properties": {
                "execution_risk": {
                    "type": "string", "enum": ["Low", "Medium", "High", "Critical"]
                },
                "financial_reporting_risk": {
                    "type": "string", "enum": ["Low", "Medium", "High", "Critical"]
                },
                "fraud_pressure_risk": {
                    "type": "string", "enum": ["Low", "Medium", "High", "Critical"]
                },
            },
        },
        "red_flags": {
            "type": "array",
            "items": {
                "type": "object", "additionalProperties": False,
                "required": ["type", "description", "basis"],
                "properties": {
                    "type": {"type": "string"},
                    "description": {"type": "string"},
                    "basis": {"type": "string"},
                },
            },
        },
        "audit_observation": {"type": "string"},
        "recommended_controls": {"type": "array", "items": {"type": "string"}},
    },
}
