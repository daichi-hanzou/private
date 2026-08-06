# AgentLedger Code Walkthrough

This walkthrough follows one raw audit event stream into the Action-centered
Explorer. Read it with `tests/test_agentledger.py`, which contains the smallest
complete examples.

## Step 1: Ingest JSONL

**File:** `src/agentledger/ingestion.py`

**Entry point:** `read_jsonl(path)`

Each non-empty line is decoded independently. JSON objects are retained;
malformed JSON and non-object values become `IngestionIssue` entries instead of
stopping the entire load.

**Input:** A path to `audit_events.jsonl`.

**Output:** `IngestionResult(events, issues, empty_lines)`.

**Read with:** `test_jsonl_reader_skips_empty_and_malformed_lines` in
`tests/test_agentledger.py`.

## Step 2: Normalize raw events

**File:** `src/agentledger/normalizer.py`

**Entry points:** `AuditEventNormalizer.normalize(...)` and
`normalize_events(...)`

The normalizer maps alternate raw field names into `NormalizedAuditEvent`,
extracts action parameters and case IDs, parses timestamps, normalizes known
outcome aliases, and preserves the complete original dictionary as
`raw_event`.

`normalize_events(...)` also makes duplicate normalized event IDs unique and
sorts events by timestamp, day, and source line.

Applications can subclass `AuditEventNormalizer`. Circular Coffee defines
`CoffeeBenchAuditAdapter` in
`src/circular_coffee/agentledger_adapter.py` for domain names and summaries.

**Read with:** normalization and duplicate-ID tests near the start of
`tests/test_agentledger.py`.

## Step 3: Resolve general event scopes

**Files:** `src/agentledger/query.py` and `src/agentledger/cases.py`

`filter_events(...)` applies `AuditQuery` fields to normalized events.
`related_case(...)` expands a proposal/case selection through IDs and
correlations. `decision_case(...)` gathers the events associated with one
Decision turn.

The CLI performs this event filtering before bundle construction. This is
useful for small scopes, but an overly narrow event-type filter can remove
Observation or Outcome context. A future bundle-first query path should address
that limitation.

**Read with:** filter, proposal-case, and decision-case tests in
`tests/test_agentledger.py`.

## Step 4: Build Action Bundles

**File:** `src/agentledger/bundles.py`

**Entry point:** `build_action_bundles(events)`

The function first indexes Observations, Decisions, Outcomes, and Human
Interventions. It then creates one `ActionBundle` for every
`action_executed` event.

An `ActionBundle` contains:

```text
observation
decision
action
outcome
human_interventions
execution_context
```

**Read with:** `test_action_bundles_join_the_four_event_types` and the Phase 2
tests at the end of `tests/test_agentledger.py`.

## Step 5: Link Observation, Decision, and Action

**File:** `src/agentledger/bundles.py`

Inside `build_action_bundles(...)`:

1. `latest_observation[correlation_id]` tracks the latest visible state for a
   turn.
2. A Decision is stored by `decision_id`.
3. The Decision captures the Observation with the same `correlation_id`.
4. An Action finds its Decision using `decision_id`.
5. If the Decision-to-Observation link is unavailable, the Action correlation
   supplies the Observation fallback.

This relationship logic is independent of event adjacency in the JSONL file.

## Step 6: Select the latest Outcome

**File:** `src/agentledger/bundles.py`

**Helpers:** `_keep_latest(...)`, `_event_order(...)`, `outcome_status(...)`,
`action_time(...)`, and `observed_at(...)`

Outcome updates are indexed by `action_id`; `decision_id` is the fallback. The
latest update is selected by:

1. `observed_at`
2. event `timestamp`
3. day
4. source line

The Action timestamp and Outcome observation timestamp remain distinct.
Missing Outcome events display as `pending`. Existing Outcomes without an
explicit lifecycle status display as `confirmed`.

**Read with:** `test_latest_asynchronous_outcome_is_used`,
`test_pending_and_unknown_outcome_statuses`, and
`test_legacy_outcome_without_status_defaults_to_confirmed`.

## Step 7: Attach Human Interventions

**Files:** `src/agentledger/models.py` and `src/agentledger/bundles.py`

`HumanInterventionEvent` stores intervention type, actor, before/after values,
reason, input method, and performed time. `_human_intervention(...)` converts a
normalized raw event, and `build_action_bundles(...)` joins it through
`related_action_id`.

The intervention remains independent from Outcome. The Explorer hides the
section when a bundle has no intervention.

**Read with:** `test_human_intervention_is_attached_and_rendered_separately`
and `test_explorer_hides_empty_human_intervention_section`.

## Step 8: Extract Execution Context

**Files:** `src/agentledger/models.py` and `src/agentledger/bundles.py`

`ExecutionContext.from_value(...)` accepts optional model, prompt, tool,
configuration, Git, and environment metadata. `_execution_context(...)` reads:

1. `action_event["execution_context"]`
2. `action_event["metadata"]["execution_context"]`

Missing values become `None`. The Explorer renders this metadata in a collapsed
panel.

**Read with:**
`test_execution_context_is_rendered_and_empty_context_is_collapsed`.

## Step 9: Generate display IDs and labels

**Files:** `src/agentledger/display.py` and `src/agentledger/bundles.py`

`assign_display_ids(...)` creates event-oriented `E`, `D`, `A`, and `P` maps
used by event-scoped sequence code. The Action Explorer currently uses
`display_action_ids(...)` and `display_case_ids(...)` from `bundles.py`.

`action_summary(...)`, `target_summary(...)`, and `trade_direction(...)`
prepare business-facing Action List values.

The two display-ID paths are a known consolidation opportunity.

**Read with:** display ID and Action summary tests in
`tests/test_agentledger.py` and `volume_test/tests/test_volume_explorer.py`.

## Step 10: Search and filtering

There are two filtering stages:

1. **CLI/event filtering:** `AuditQuery` and `filter_events(...)` in
   `src/agentledger/query.py`.
2. **Explorer/Action filtering:** embedded JavaScript generated by
   `src/agentledger/html.py`.

The Explorer creates `search_text` for every bundle and supports global search
plus day, Action ID, agent, Action type, target, and summary column filters.

**Read with:** query tests and HTML filter tests in
`tests/test_agentledger.py`.

## Step 11: Assemble the Explorer

**File:** `src/agentledger/html.py`

**Entry points:**

- `render_explorer(...)`
- `render_action_bundles(...)`
- `write_explorer(...)`

`_bundle_payload(...)` converts each `ActionBundle` into serializable data.
`render_action_bundles(...)` embeds that data, CSS, and JavaScript into one HTML
document. `write_explorer(...)` writes the document to disk.

The detail panel renders Observation, Decision, Action, optional Human
Intervention, collapsed Execution Context, and Outcome. The sequence panel uses
`build_action_sequence(...)` from `src/agentledger/sequence.py`.

Mermaid is fetched from a CDN at view time. If unavailable, the generated
Mermaid source is displayed as text. No application backend is required.

**Read with:** Explorer and HTML UI tests in `tests/test_agentledger.py`.

## Step 12: Use the CLI

**File:** `src/agentledger/cli.py`

**Entry point:** `main()`

The CLI combines ingestion, normalization, optional event filtering, case
expansion, bundle construction, and HTML output:

```bash
uv run agentledger build \
  path/to/audit_events.jsonl \
  --output outputs/agentledger.html
```

No console-script entry point is currently declared in `pyproject.toml`, so
`uv run agentledger` is the canonical repository command. The module form,
`uv run python -m agentledger.cli`, remains available as a fallback.

## Step 13: Follow CoffeeBench event emission

**Primary file:** `src/circular_coffee/simulation.py`

`SimulationRunner` calls `_audit_event(...)` when an agent receives an
Observation, makes a Decision, executes an Action, or observes an Outcome.
That helper delegates to `AuditLogger.log_event(...)` in
`src/circular_coffee/audit/logger.py`.

Payload shaping for Observation and Decision events lives in
`src/circular_coffee/audit/schemas.py`. The resulting file is
`audit_events.jsonl` in the run output directory.

The CoffeeBench-specific normalizer extension is
`src/circular_coffee/agentledger_adapter.py`, although the generic AgentLedger
CLI currently uses the base normalizer.

**Read with:**

- `tests/test_audit.py` for event emission, correlation, failure isolation, and
  audit-enabled/disabled equivalence
- `tests/test_audit_sequence.py` for the older CoffeeBench replay/sequence path
- `tests/test_agentledger.py` for generic ingestion and Explorer behavior

## Step 14: Check volume behavior

**Files:** `volume_test/generate_volume_data.py`,
`volume_test/benchmark_generation.py`, `volume_test/benchmark_browser.py`, and
`volume_test/reporting.py`

The generator creates complete four-event Action lifecycles. Generation tests
cover 100, 1,000, and 10,000 Actions. Benchmark scripts measure normalization,
bundle construction, HTML generation, file size, and browser interactions.

Run:

```bash
uv run python -m pytest volume_test/tests -q
uv run python -m volume_test.benchmark_generation --actions 100
```

Generated HTML and result files are ignored by Git.
