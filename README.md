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

The local-provider command above does not connect to Gmail, Microsoft Graph,
Google Calendar, Outlook Calendar, LINE, or an LLM. Calendar candidates with
complete dates and times can be converted to the existing `CalendarRequest`;
ambiguous candidates and deadlines without explicit times are rejected at that
boundary rather than receiving invented values.

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

The `process` command remains fully local. Its default analysis mode is the
hybrid rule/Ollama mode described below; pass `--analysis-mode rule-only` for
the original deterministic behavior. Gmail and real calendar APIs, LINE, and
OpenClaw are not connected. Providers replace
`LocalMailProvider` through `MailProvider`; the same orchestration service can
later be invoked from another execution surface such as an OpenClaw Skill.

## Personal Outlook read-only provider

`OutlookProvider` can explicitly fetch a small number of messages from a
personal Outlook.com Inbox through Microsoft Graph. It uses an MSAL public
client with device code flow and the delegated `Mail.Read` permission only.
There is no client secret, application permission, mailbox mutation method, or
calendar API call. The HTTP transport exposes GET only.

The `msal` dependency is used so OAuth token acquisition is not implemented by
this repository. The lightweight `requests` dependency is used instead of the
larger Microsoft Graph SDK because this provider only needs read-only message
GET requests. See Microsoft's documentation for [app
registration](https://learn.microsoft.com/en-us/entra/identity-platform/quickstart-register-app),
[device code flow](https://learn.microsoft.com/en-us/entra/identity-platform/scenario-desktop-acquire-token-device-code-flow),
[Mail.Read](https://learn.microsoft.com/en-us/graph/permissions-reference#mailread),
and [listing messages](https://learn.microsoft.com/en-us/graph/api/user-list-messages?view=graph-rest-1.0).

### One-time Microsoft Entra setup

1. In Microsoft Entra admin center, create an App registration.
2. Select a supported account type that includes personal Microsoft accounts.
   For personal-only use, configure the personal Microsoft account audience;
   for a combined registration, select organizational directories and personal
   Microsoft accounts.
3. Copy the Application (client) ID. Do not create a client secret.
4. Under Authentication, enable the public client flow required for device
   code authentication.
5. Under API permissions, add Microsoft Graph delegated `Mail.Read`. Do not add
   `Mail.ReadWrite`, `Mail.Send`, calendar, file, contact, admin, or application
   permissions.
6. Set the client ID locally, for example:

   ```bash
   export AGENTLEDGER_MICROSOFT_CLIENT_ID="<APPLICATION_CLIENT_ID>"
   ```

The first CLI run prints Microsoft's verification URL and device code. Complete
sign-in with the personal Microsoft account whose mail should be read. Later
runs first attempt silent acquisition from the local MSAL cache.

The default cache is
`~/.config/agentledger/microsoft_token_cache.json`. It is written atomically
with mode `0600` where supported. The cache contains sensitive authentication
material: never commit, print, attach, or share it. The repository ignores
Microsoft token-cache filename patterns. MSAL's serializable file cache is a
minimal local persistence mechanism; users needing OS-protected encrypted
storage should use an appropriate secure cache integration in a future change.

Start with at most five unread messages:

```bash
uv run mail-calendar-orchestrator outlook \
  --client-id "$AGENTLEDGER_MICROSOFT_CLIENT_ID" \
  --token-cache ~/.config/agentledger/microsoft_token_cache.json \
  --folder inbox \
  --max-messages 5 \
  --unread-only \
  --received-after "2026-08-01T00:00:00+09:00" \
  --output /tmp/outlook-mail-calendar.jsonl \
  --base-year 2026 \
  --timezone Asia/Tokyo \
  --requires-approval

uv run agentledger build \
  /tmp/outlook-mail-calendar.jsonl \
  --output /tmp/outlook-mail-calendar.html
```

The CLI states that it is read-only and masks the authenticated address. It
does not print tokens or message bodies. `max-messages` must be between 1 and
100; unread-only is the default. Graph paging follows only HTTPS next links on
`graph.microsoft.com`, stops at the message/page limits, detects loops, and
performs bounded retries for throttling and server errors.

HTML bodies are converted locally without executing scripts, loading images,
or following URLs. Script/style elements are removed, text is size-limited,
and the existing audit layer still stores only the capped preview—not the full
body, Graph response, provider metadata, tokens, headers, or attachments.

This command produces local audit JSONL and pending local Calendar proposals;
it does not mark mail as read or change mail/calendar state. Real calendar
registration, Gmail, LINE, LLM, OpenClaw, and periodic monitoring remain
unimplemented except for the optional local Ollama analysis described below.
Access can be revoked later from the Microsoft account's app
permission/privacy management page.

## Local Ollama/Qwen-assisted mail analysis

Mail analysis uses a locally running Qwen model for semantic understanding and
a deterministic Validator for factual and safety checks. The default and
recommended operational mode is `llm-first`: every email is normally analyzed
by the LLM, but only a validated `calendar_candidate` may reach Calendar Agent.
`rule-only` is for deterministic comparison or LLM outages. `hybrid` preserves
the earlier comparison behavior and can still pass a high-confidence rule
false positive without LLM review. `llm-all` is a fail-closed experimental
comparison mode. None of these modes bypasses Validator when an LLM result is
used.

The defaults are `http://localhost:11434`, `qwen3:8b`, a 120-second timeout,
`temperature=0`, `stream=false`, `think=false`, and `keep_alive=5m`.
Thinking is disabled for mail analysis because Qwen 3 otherwise emits a
separate, potentially long reasoning trace. `--ollama-thinking` can enable it
for diagnostics, but the trace is never parsed or written to AgentLedger;
`--no-ollama-thinking` is the default. Models that reject the setting follow
the normal safe fallback/error path. `qwen3:4b` can be selected for smaller
machines. The client uses the existing `requests` dependency and
Ollama's native [`POST /api/chat`](https://docs.ollama.com/api/chat), passes a
JSON Schema in `format` as documented for [structured
outputs](https://docs.ollama.com/capabilities/structured-outputs), and uses the
official [`GET /api/tags`](https://docs.ollama.com/api/tags) model list for the
connection check. No Ollama Python dependency is added.

Start Ollama and confirm the model before processing mail:

```bash
ollama list
ollama run qwen3:8b

uv run mail-calendar-orchestrator check-llm \
  --ollama-model qwen3:8b
```

The check performs both model discovery and a short structured-output email
probe, and reports the probe latency. Model absence, timeout, and invalid JSON
are reported as distinct errors rather than treating API reachability alone as
success.

For the first real-mail comparison, generate analysis events only—no Calendar
Agent proposal Actions:

```bash
uv run mail-calendar-orchestrator outlook \
  --client-id "$AGENTLEDGER_MICROSOFT_CLIENT_ID" \
  --max-messages 5 \
  --unread-only \
  --analysis-mode llm-first \
  --analysis-only \
  --ollama-model qwen3:8b \
  --output /tmp/outlook-llm-analysis.jsonl \
  --base-year 2026 \
  --timezone Asia/Tokyo

uv run agentledger build \
  /tmp/outlook-llm-analysis.jsonl \
  --output /tmp/outlook-llm-analysis.html
```

After reviewing the rule/LLM decisions in Explorer, omit `--analysis-only` and
keep `--requires-approval` to create pending local proposals. Nothing writes to
an actual calendar.

Only one email is analyzed per Ollama request. The input contains sender,
subject, received time, bounded body text (6000 characters by default, maximum
20000), timezone, base year, compact rule results, importance/categories, and
the attachment-presence flag. It never includes Microsoft tokens, token cache,
Graph headers/responses, attachment contents, unrelated messages, or the
AgentLedger log. With the default localhost URL, email content stays on the
local machine. A non-loopback Ollama host is rejected unless
`--allow-remote-ollama` is explicitly supplied.

Email subject/body are placed only in the user message inside explicit
untrusted-data delimiters. The system prompt tells the model to ignore email
instructions and forbids tools, files, URLs, sending mail, and calendar
changes. Structured output is revalidated for exact fields, enum/numeric/size
limits, ISO date/time syntax, short evidence that occurs in the original
email, user-commitment evidence, and grounded dates, times, duration, location,
and title. Relative or conflicting dates and low confidence (default threshold
`0.75`) become clarification only when a realistic personal calendar item is
possible. Invented facts become invalid rather than clarification.
Here, a calendar candidate means an item the user may add to their own personal
calendar—not registration for or RSVP to an event. Public seminars and event
advertisements require explicit evidence that the user intends to attend;
security notices, promotions, and general information are not scheduled by
default.

Every LLM analysis ends in exactly one auditable classification:
`calendar_candidate`, `clarification_required`, `informational`, `promotion`,
`security_notification`, `ignored`, or `invalid`. General seminar/webinar
advertising remains `promotion` or `informational` even when it contains a
date. New sign-ins, app connections, security codes, password changes, and
suspicious-access notices become `security_notification`; they are not sent to
Calendar Agent or to clarification. Clarification is reserved for cases such
as a personal meeting whose date or participation details remain genuinely
ambiguous.

Validator distinguishes the model's `llm_proposed_classification` from the
normalized `final_classification` and records `classification_corrections`.
Sentinel security types such as `"none"` are treated as null. A security
classification requires a security category, a real notification type, or a
deterministically recognized security phrase. Non-candidates
(`candidate_type=none` and `should_create_calendar_candidate=false`) do not run
irrelevant date/time/duration grounding checks.

In `llm-first`, connection refusal, timeout, missing model, HTTP failure, empty or
oversized response, invalid JSON, and schema failure fall back conservatively
to `invalid` without stopping the batch or accepting the rule result as a
semantic decision. `hybrid` retains its rule fallback for comparison.
`--require-llm` makes an invoked LLM failure fatal;
`llm-all` is also fail-closed. Environment overrides are
`AGENTLEDGER_OLLAMA_BASE_URL` and `AGENTLEDGER_OLLAMA_MODEL`.

AgentLedger records the mode, whether the LLM was used, model name, prompt
template/schema/config hashes and versions, final classification, user
commitment detection, generic-event flag, security-notification type,
candidate rejection reason, confidence, final source, validation issues,
fallback reason, suspicious-instruction flag, latency, input truncation, and
compact rule/LLM summaries. It does not record the full
body, completed prompt, raw model response, access tokens, token cache, Graph
response, or authorization headers. Prompt hashes cover templates and schema,
not the body-bearing completed prompt.

Cloud LLMs, Gmail, real calendar writes, LINE approval, OpenClaw, automatic
model downloads, training, and a persistent LLM cache are not part of this
implementation.

Calendar proposals still require human approval. Meaning is decided by the
local LLM, facts are checked by Validator, and external calendar operations
remain unimplemented.

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
