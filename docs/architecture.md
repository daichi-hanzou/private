# AgentLedger Architecture

This document describes the repository as it exists today. It is not a proposal
for a directory refactor.

## System boundary

```text
src/agentledger/
    Generic ingestion, normalization, relationship, bundle, query, display,
    sequence, and static Explorer logic

src/circular_coffee/
    Coffee trading simulation, domain events, metrics, policies, audit
    emission, and integration-specific formatting
```

AgentLedger does not know about coffee lots, Roasters, Retailers, trading
prices, or circular-trade metrics. Circular Coffee emits generic audit events
and has an optional `CoffeeBenchAuditAdapter` for domain-specific names and
summaries.

## AgentLedger modules

### `models.py`

**Responsibility:** Typed normalized data structures.

**Main types:**

- `NormalizedAuditEvent`
- `HumanInterventionEvent`
- `ExecutionContext`
- `IngestionIssue`
- `IngestionResult`

**Input:** Values supplied by ingestion and normalization.

**Output:** Immutable dataclasses consumed by all later layers.

**Dependencies:** Python dataclasses, `datetime`, and generic `Any` payloads.

### `ingestion.py`

**Responsibility:** Read append-oriented JSONL safely.

**Main function:** `read_jsonl(path)`

**Input:** A filesystem path.

**Output:** `IngestionResult` containing JSON-object events, malformed-line
issues, and an empty-line count.

**Dependencies:** `models.py`. It does not apply domain semantics.

### `normalizer.py`

**Responsibility:** Convert heterogeneous raw dictionaries into
`NormalizedAuditEvent` records and derive common fields.

**Main API:**

- `AuditEventNormalizer.normalize(...)`
- `normalize_events(...)`
- `normalize_outcome(...)`
- `searchable_text(...)`
- `humanize(...)`

**Input:** Raw event dictionaries.

**Output:** Stable, timestamp-sorted normalized events with unique normalized
event IDs. The original dictionary remains in `raw_event`.

**Dependencies:** `models.py`. Applications can subclass
`AuditEventNormalizer`; `CoffeeBenchAuditAdapter` does this for display names
and action summaries.

### `bundles.py`

**Responsibility:** Join normalized events into Action-centered audit bundles
and derive Action display values.

**Main API:**

- `ActionBundle`
- `build_action_bundles(...)`
- `display_action_ids(...)`
- `display_case_ids(...)`
- `action_summary(...)`
- `business_state(...)`
- `outcome_status(...)`
- `action_time(...)`
- `observed_at(...)`

**Input:** A list of normalized events.

**Output:** One `ActionBundle` per `action_executed` event, containing its
Observation, Decision, latest Outcome, Human Interventions, and Execution
Context.

**Dependencies:** `models.py` and normalization display helpers.

### `query.py`

**Responsibility:** Filter normalized event collections.

**Main API:** `AuditQuery`, `filter_events(...)`

**Input:** Normalized events and an `AuditQuery`.

**Output:** Events matching run, day, agent, event/action/case type, IDs,
status, counterparty, and keyword criteria.

**Dependencies:** `normalizer.searchable_text`.

### `display.py`

**Responsibility:** Assign compact IDs for event-oriented display.

**Main API:** `DisplayIds`, `assign_display_ids(...)`

**Input:** Normalized events.

**Output:** Stable maps such as `E1`, `D1`, `A1`, and `P1`.

**Dependencies:** `models.py`.

The Action-centered Explorer also assigns Action and case IDs through
`bundles.py`. This overlapping responsibility is a current refactor candidate.

### `cases.py`

**Responsibility:** Expand an event selection to a related proposal or decision
scope.

**Main API:** `related_case(...)`, `decision_case(...)`

**Input:** Normalized events and a proposal/case or decision identifier.

**Output:** The related Observation, Decision, Action, and Outcome events found
through case IDs, correlation IDs, decision IDs, and action IDs.

**Dependencies:** `models.py`.

### `sequence.py`

**Responsibility:** Build Mermaid sequence source.

**Main API:** `build_action_sequence(...)`, `build_sequence(...)`

**Input:** An `ActionBundle` for the current Explorer, or an event list for the
older event-scoped business/technical builders.

**Output:** Mermaid `sequenceDiagram` text.

**Dependencies:** Bundle display helpers, display IDs, models, and normalizer
helpers. The Action Explorer uses the business-oriented
`build_action_sequence(...)` path and does not show Environment as a
participant.

### `html.py`

**Responsibility:** Serialize Action Bundles and assemble the static Explorer.

**Main API:**

- `render_explorer(...)`
- `render_action_bundles(...)`
- `write_explorer(...)`

**Input:** Normalized events or prebuilt Action Bundles.

**Output:** A single HTML document containing embedded audit data, table
filters, detail panels, and Mermaid source.

**Dependencies:** `bundles.py`, `normalizer.py`, and `sequence.py`.

### `cli.py`

**Responsibility:** Expose static Explorer generation as a command-line
workflow.

**Main entry point:** `main()`

**Input:** An audit JSONL path, output path, and optional event filters.

**Output:** An Explorer HTML file plus loaded/skipped/selected counts.

**Dependencies:** Ingestion, normalization, query, case expansion, bundle
construction, and HTML output.

## End-to-end data flow

```text
JSONL line
    ↓
read_jsonl(...)
    ↓
IngestionResult.events
    ↓
AuditEventNormalizer / normalize_events(...)
    ↓
NormalizedAuditEvent
    ↓
filter_events(...) and optional related_case(...) / decision_case(...)
    ↓
build_action_bundles(...)
    ↓
ActionBundle
    ↓
display helpers + build_action_sequence(...)
    ↓
render_action_bundles(...) / write_explorer(...)
    ↓
Standalone Explorer HTML
```

The CLI currently filters events before constructing bundles. A narrow filter
can therefore exclude context events required to form a complete bundle. This
is documented behavior and a candidate for a later structural change.

## Bundle relationship rules

`build_action_bundles(...)` performs a linear indexing pass and then creates
one bundle for each Action.

1. **Observation to Decision:** the latest `observation_received` event for a
   `correlation_id` is associated with a `decision_made` event using the same
   correlation.
2. **Decision to Action:** an `action_executed` event resolves its Decision
   through `decision_id`. Its Observation comes from that Decision association,
   falling back to the latest Observation for the Action correlation.
3. **Outcome to Action:** `outcome_observed` is indexed by `action_id`.
   `decision_id` is a fallback when no Action-linked Outcome exists.
4. **Latest Outcome:** when multiple Outcomes share an identifier, the most
   recent applicable event replaces the previous indexed value.
5. **Human Intervention to Action:** a `human_intervention` event uses
   `related_action_id` (or its normalized `action_id` fallback). A bundle checks
   both the Action's external `action_id` and its normalized event ID.
6. **Execution Context:** the builder reads `execution_context` from the Action
   event root, then from `metadata.execution_context`. Missing fields remain
   `None`.

## Asynchronous outcomes

Outcome history remains append-oriented in the input log. AgentLedger does not
rewrite an earlier event. Instead, it selects the latest current state for each
Action Bundle:

- multiple `outcome_observed` events may reference one Action;
- `observed_at` is preferred when ordering updates;
- the event `timestamp`, day, and source line provide deterministic fallbacks;
- `action_time` is read from `action_time` or `executed_at`, then falls back to
  the Action event timestamp for display;
- displayed observed time uses `observed_at`, then the Outcome event timestamp;
- an Action with no Outcome is shown as `pending`;
- an existing Outcome without an explicit lifecycle `status` is shown as
  `confirmed` for CoffeeBench compatibility;
- an unsupported explicit lifecycle status is shown as `unknown`.

The generic lifecycle status and the domain-specific `business_state` are
separate. For example, an old CoffeeBench Outcome can be confirmed as an
observed record while its trade proposal remains pending.

## Circular Coffee integration

`SimulationRunner` emits `observation_received`, `decision_made`,
`action_executed`, and `outcome_observed` events through
`SimulationRunner._audit_event(...)`.

```text
SimulationRunner
    ↓
AuditLogger.log_event(...)
    ↓
audit_events.jsonl
    ↓
AgentLedger ingestion and normalization
```

Relevant integration files:

- `src/circular_coffee/simulation.py`: determines when events are emitted.
- `src/circular_coffee/audit/logger.py`: append-only JSONL writer.
- `src/circular_coffee/audit/schemas.py`: JSON-safe Observation and Decision
  payload builders.
- `src/circular_coffee/agentledger_adapter.py`: CoffeeBench names and summary
  formatting through `CoffeeBenchAuditAdapter`.

The generic CLI currently uses the base `AuditEventNormalizer` directly; it
does not automatically select `CoffeeBenchAuditAdapter`. CoffeeBench also
retains its earlier replay and sequence modules under
`src/circular_coffee/audit/`. Consolidating those paths is deferred.

## Current coupling and boundaries

Current boundaries are mostly explicit, but several responsibilities overlap:

- `display.py` and `bundles.py` both assign compact display IDs for different
  rendering paths.
- `sequence.py` contains both Action-centered and older event-centered sequence
  builders.
- Circular Coffee retains domain-specific replay and sequence rendering in
  addition to AgentLedger.
- The CoffeeBench adapter exists but is not automatically selected by the
  generic CLI.
- Querying is event-first, while the Explorer is Action Bundle-first.

These are architectural observations, not changes made by this documentation
update.

## Future architecture

The intended direction is:

```text
Append-only Event Store
          ↓
Bundle / Query Layer
       ↙          ↘
 Explorer       Analyzer
```

The repository currently uses JSONL files rather than a generalized Event
Store, and the Analyzer has not been implemented.
