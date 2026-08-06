# AgentLedger

**A decision-centric audit and exploration framework for AI agents.**

AgentLedger records and connects the semantic lifecycle of an agent action:

```text
Observation
    ↓
Decision
    ↓
Action
    ↓
Outcome
```

Real operations may also include a human intervention and an outcome that
arrives well after the action. AgentLedger is therefore more than a log
viewer: it is an audit layer for understanding what an agent observed, why it
made a decision, what it did, what happened afterward, and which execution
context produced that behavior.

Circular Coffee (CoffeeBench) is the first integration and validation
environment in this repository. It produces realistic multi-agent audit logs,
but it is not the AgentLedger core.

## Current capabilities

- JSONL ingestion with malformed-line reporting
- Normalization into a common audit event model
- Action Bundle construction across Observation, Decision, Action, and Outcome
- Human Intervention events associated with Actions
- Asynchronous Outcome updates with latest-state selection
- Execution Context metadata for model, prompt, tool, configuration, code, and
  environment versions
- Action-centered Explorer with search and column filtering
- Business sequence diagrams
- Standalone HTML export with embedded audit data and no backend requirement
- Backward compatibility with existing CoffeeBench audit logs
- Generation and browser benchmark tooling through 10,000 Actions

The current Explorer uses a full HTML table. It has been benchmarked with
10,000 Actions; this is a measured test scale, not an unlimited scalability
claim.

## Event model

| Component | Meaning |
|---|---|
| Observation | The state and options visible to the agent |
| Decision | The selected action, explanation, and optional expected outcome |
| Action | The operation submitted or executed by the agent |
| Outcome | The latest observed result of the Action |
| Human Intervention | An independent event associated with an Action |
| Execution Context | Optional Action Bundle metadata describing the runtime |

Supported Outcome lifecycle statuses are:

```text
pending
confirmed
failed
contradicted
expired
unknown
```

An Outcome may begin as `pending` and later be superseded by a newer Outcome
event. Existing logs whose Outcome has no explicit lifecycle status are treated
as `confirmed` for backward compatibility. Business-specific state, such as a
pending trade proposal, remains separately visible in the Explorer.

Human Intervention values can describe actions such as `accept`, `reject`,
`modify`, `undo`, or `ignored`. The current core preserves these values rather
than enforcing a closed enum.

Execution Context belongs to the Action Bundle and may contain:

```text
model_name
model_version
prompt_hash
tool_version
config_hash
git_commit
environment
```

All fields are optional.

## Architecture

```text
Application / Agent
        ↓
Audit Events
        ↓
AgentLedger Normalizer
        ↓
Action Bundle Builder
        ↓
Query / Display Layer
        ↓
Explorer HTML
```

The current integration follows the same path:

```text
Circular Coffee
        ↓
agentledger_adapter
        ↓
AgentLedger
        ↓
Explorer
```

Future calendar, email, and home-automation integrations are expected to reuse
the same AgentLedger core. See [Architecture](docs/architecture.md) and the
[Code Walkthrough](docs/code-walkthrough.md) for the current implementation.

## Repository layout

```text
src/agentledger/
    Reusable AgentLedger core

src/circular_coffee/
    Circular Coffee simulation and AgentLedger integration

scripts/
    Simulation, replay, comparison, and visualization scripts

tests/
    Unit and integration tests

volume_test/
    Large-volume generation and browser benchmark tooling
```

## Quick start

Python 3.11 or newer and `uv` are required.

```bash
uv sync
uv run pytest
```

Build an AgentLedger Explorer from an existing audit log:

```bash
uv run agentledger build \
  path/to/audit_events.jsonl \
  --output outputs/agentledger.html
```

Try the minimal pending and confirmed audit flows:

```bash
uv run agentledger build \
  examples/minimal_audit_pending.jsonl \
  --output /tmp/agentledger-pending.html
uv run agentledger build \
  examples/minimal_audit_confirmed.jsonl \
  --output /tmp/agentledger-confirmed.html
```

The command accepts filters such as `--day`, `--agent`, `--action-type`,
`--proposal-id`, `--decision-id`, and `--search`.

## Calendar Agent demo

The local Calendar Agent is a deterministic, rule-based integration example.
It does not call Google Calendar, Gmail, an LLM, or any external service.

```bash
uv run calendar-agent propose \
  --title "Dental appointment" \
  --date "2026-08-12" \
  --preferred-period afternoon \
  --duration 60 \
  --slots 13:00 15:00 \
  --requires-approval \
  --output /tmp/calendar-pending.jsonl

uv run agentledger build \
  /tmp/calendar-pending.jsonl \
  --output /tmp/calendar-pending.html

uv run calendar-agent approve \
  --input /tmp/calendar-pending.jsonl \
  --action-id <ACTION_ID> \
  --actor daichi \
  --reason "Approved" \
  --output /tmp/calendar-approved.jsonl

uv run agentledger build \
  /tmp/calendar-approved.jsonl \
  --output /tmp/calendar-approved.html
```

An approval-required proposal contains Observation, Decision, and Action and
appears as `pending` until `approve` or `reject` writes a separate output log.
Use `--no-requires-approval` for an immediate confirmed Outcome. The `reject`
command accepts the same arguments as `approve` and records a contradicted
Outcome. Resolution stops without writing output if the input has malformed
lines, the Action is missing or ambiguous, or it already has an Outcome.

## Mail-to-Calendar Agent demo

The Mail-to-Calendar Agent converts provider-neutral email records into
calendar candidates and AgentLedger audit events. The current implementation
uses deterministic rules and `LocalMailProvider`; future Gmail and Outlook
integrations can implement the small `MailProvider` interface without changing
classification or extraction logic.

```bash
uv run mail-to-calendar process \
  --input examples/mail_to_calendar/sample_messages.jsonl \
  --output /tmp/mail-to-calendar.jsonl \
  --base-year 2026 \
  --timezone Asia/Tokyo

uv run agentledger build \
  /tmp/mail-to-calendar.jsonl \
  --output /tmp/mail-to-calendar.html
```

The service uses full `body_text` only while applying local rules. Audit logs
contain a maximum 160-character provider preview, source message ID, and
structured classification/candidate fields; they do not contain the full mail
body or provider-specific metadata. Attachment content and filenames are not
loaded.

This demo does not connect to Gmail, Microsoft Graph, Google Calendar,
Outlook Calendar, LINE, or an LLM. Calendar candidates with complete dates and
times can be converted to the existing `CalendarRequest`; ambiguous candidates
and deadlines without explicit times are rejected at that boundary rather
than receiving invented values.

## Mail-to-Calendar Orchestrator demo

The Mail-to-Calendar Agent classifies mail and extracts candidates; the
Calendar Agent manages calendar proposals and their approval lifecycle. The
Orchestrator connects those existing Python services directly, without calling
either CLI as a subprocess:

```text
LocalMailProvider
    → MailToCalendarService
    → CalendarCandidate
    → CalendarRequest
    → CalendarAgent.propose
    → combined AgentLedger JSONL
```

Run the approval-required flow:

```bash
uv run mail-calendar-orchestrator process \
  --input examples/mail_to_calendar/sample_messages.jsonl \
  --output /tmp/mail-calendar-flow.jsonl \
  --base-year 2026 \
  --timezone Asia/Tokyo \
  --requires-approval

uv run agentledger build \
  /tmp/mail-calendar-flow.jsonl \
  --output /tmp/mail-calendar-flow.html
```

Use `--no-requires-approval` to include confirmed Calendar Outcomes in the
same run. With approval enabled, each ready Calendar Action has no Outcome and
appears as `pending` in the Explorer.

Candidates are classified into four non-overlapping orchestration states:

- `ready`: complete date, start time, and duration; sent to Calendar Agent
- `clarification_required`: explicitly ambiguous; remains in the mail flow
- `unsupported`: unsafe conversion such as a deadline without a time
- `ignored`: non-important mail; never sent to Calendar Agent

Calendar events retain the source provider/message/thread IDs, candidate ID,
and parent Mail Action/Decision/Correlation IDs as metadata. Full mail bodies,
provider-specific metadata, attachments, and attachment names are not copied
into the combined log. Output is written through an atomic replacement after
the complete batch succeeds.

This remains a local rule-based demo. Gmail, Microsoft Graph, real calendar
APIs, LINE, LLMs, and OpenClaw are not connected. A future Gmail or Outlook
adapter can replace `LocalMailProvider` through `MailProvider`; the same
orchestration service can later be invoked from another execution surface such
as an OpenClaw Skill.

Run the AgentLedger volume smoke tests:

```bash
uv run python -m pytest volume_test/tests -q
```

Generate a small benchmark report without external API credentials:

```bash
uv run python -m volume_test.benchmark_generation --actions 100
```

## Explorer

The Explorer is centered on Actions rather than raw event rows.

```text
Action List
    ↓
Action Detail
    ├── Observation
    ├── Decision
    ├── Action
    ├── Human Intervention (when present)
    ├── Execution Context
    └── Outcome

Business Sequence
```

The generated HTML embeds normalized data and does not need an application
server. Mermaid is loaded from a CDN for rendered sequence diagrams; when it is
unavailable, the Mermaid source is shown as a text fallback.

## Roadmap

These items are future work, not current functionality:

- Real-world calendar and email integrations
- Long-term operational logging
- Human-override analysis
- Delayed-outcome analysis
- Version-to-version decision comparison
- Silent regression detection
- Failure clustering and an Analyzer
- Optional OpenTelemetry interoperability

## Circular Coffee example

Circular Coffee is a simulation environment for studying multi-agent trading,
reported-revenue incentives, consumer sales, and circular ownership paths. It
serves three roles in this repository:

- an AgentLedger integration example
- a source of audit and benchmark logs
- a controlled research environment for validating event relationships

Run the deterministic scripted scenario:

```bash
uv run python scripts/run_scripted.py
```

Run the scripted condition comparison:

```bash
uv run python scripts/run_condition_comparison.py --seed 0
```

Run a random-policy scenario:

```bash
uv run python scripts/run_random.py --seed 0 --max-days 20
```

LLM experiments require provider credentials. For OpenAI, set
`OPENAI_API_KEY`; for Azure OpenAI, set the Azure endpoint, deployment, API
version, and API key variables required by the existing runner. Credentials
must not be written to experiment logs or committed to the repository.

### Interpretation limits

Circular Coffee supports both scripted and LLM-backed policies. Results must be
described according to the policy mode actually used:

- A scripted Retailer response is controlled environment behavior, not an
  independently discovered LLM strategy.
- A Roaster-only LLM run does not show that multiple LLM agents jointly
  discovered the complete ownership cycle.
- A multi-agent LLM run should still be interpreted from the saved actions,
  proposals, trades, and audit events rather than from aggregate metrics alone.

The simulation is a research integration. Domain-specific market rules,
metrics, and cycle detection remain under `src/circular_coffee/`; generic audit
and Explorer behavior belongs under `src/agentledger/`.
