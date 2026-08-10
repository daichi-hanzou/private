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

The CLI automatically reads `AGENTLEDGER_*` settings from a `.env` file in the
current working directory. It parses the file as data without executing shell
commands and never overwrites variables already exported by the shell. For
example:

```dotenv
AGENTLEDGER_MICROSOFT_CLIENT_ID=<APPLICATION_CLIENT_ID>
AGENTLEDGER_OLLAMA_BASE_URL=http://localhost:11434
AGENTLEDGER_OLLAMA_MODEL=qwen3:8b
```

`.env` is Git-ignored and should have mode `0600`. Replace placeholders before
running the Outlook command; the CLI reports an explicit error for an unchanged
Client ID placeholder.

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

### Processed-mail state

The orchestrator uses a local SQLite database by default so repeated batch
runs do not analyze the same unchanged message or propose its calendar
candidate twice. The default path is
`~/.local/share/agentledger/mail_state.sqlite3`; override it with
`--state-db PATH`. Use `--reprocess` for deliberate evaluation reruns,
`--retry-failed` for permanently failed rows, or `--no-state` for isolated
debugging. `retryable` rows and `processing` rows older than 30 minutes are
retried automatically.

```bash
uv run mail-calendar-orchestrator outlook \
  --max-messages 20 --unread-only \
  --analysis-mode llm-first --analysis-only \
  --output /tmp/outlook-analysis.jsonl

uv run mail-calendar-orchestrator state summary
uv run mail-calendar-orchestrator state recent --limit 20
uv run mail-calendar-orchestrator state reset-message \
  --provider outlook --message-id MESSAGE_ID
```

SQLite is operational state; AgentLedger JSONL remains the decision audit log.
The database stores identifiers, hashes, classifications, action links,
versions, timestamps, and bounded errors. It does not store message bodies,
subjects, prompts, raw LLM/Graph responses, attachments, or tokens. Its parent
directory and file are created with permissions `0700` and `0600` where the
platform supports them. The database file and SQLite sidecar files are ignored
by Git.

Each fetched batch is recorded in `runs`. Messages are marked `processing`
before analysis, but become `processed` only after the JSONL atomic rename has
succeeded; temporary LLM failures remain `retryable`. State changes use SQLite
transactions per message. If there are no new messages, the command prints
`No new messages to process.`, records a completed run, and does not create or
replace the requested JSONL file.

### Scheduled Outlook polling with systemd

The optional systemd **user** timer runs the read-only Outlook (and optionally
Gmail) → local Qwen → Validator → pending Calendar Agent proposal flow at
07:30, 12:30, and 18:30
Asia/Tokyo. It never installs a root/system-wide service, writes to a real
calendar, or sends LINE notifications. `Persistent=true` lets systemd run a
missed timer after the Mini PC starts again.

On first installation, SQLite deterministically bootstraps
`processing_started_from` to 00:00 of the current day in the configured local
timezone. Older mail is not fetched. Override the initial value once with
`--start-from today` or an offset-bearing ISO timestamp. An existing value is
never replaced unless `--reset-start-from` is explicitly supplied.

Subsequent runs fetch from `last_successful_poll_at` minus five minutes, while
SQLite message state—not the cursor—remains responsible for duplicate
elimination. Change the overlap with `--poll-overlap-minutes 0..60`. The cursor
advances only after Graph fetch, analysis, atomic JSONL output, and SQLite state
updates succeed. Individual retryable LLM errors may produce
`completed_with_errors` and advance the cursor because their messages remain
`retryable`. Failed Graph/output/state operations do not advance it. When the
50-message safety cap is reached, the cursor advances only to the newest
returned message so later runs can drain the remainder.

```bash
uv run mail-calendar-orchestrator state cursor
uv run mail-calendar-orchestrator run-scheduled --generate-explorer
```

Each non-empty run writes distinct JSONL (and optionally Explorer HTML) under
`~/.local/share/agentledger/runs/`. Empty runs create no JSONL. Scheduled mode
first performs a lightweight localhost Ollama/model check without inference.
Defaults are LLM-first, Qwen `qwen3:8b`, thinking disabled, approval required,
and at most 50 messages. The unit enforces a 30-minute limit. A SQLite lock
rejects overlapping runs and recovers a stale lock. Failures return non-zero,
preserve the cursor, and do not disable the timer.

Install and manage the user units explicitly:

```bash
uv run mail-calendar-orchestrator scheduler install
# Edit ~/.config/agentledger/mail-calendar.env, then:
uv run mail-calendar-orchestrator scheduler enable
uv run mail-calendar-orchestrator scheduler status
uv run mail-calendar-orchestrator scheduler run-now
uv run mail-calendar-orchestrator scheduler disable
uv run mail-calendar-orchestrator scheduler uninstall
```

Installation creates `~/.config/systemd/user/agentledger-mail.service`,
`agentledger-mail.timer`, and the mode-`0600`
`~/.config/agentledger/mail-calendar.env`. Set
`AGENTLEDGER_MICROSOFT_CLIENT_ID` there. Optional settings are
`AGENTLEDGER_OLLAMA_MODEL`, `AGENTLEDGER_OLLAMA_BASE_URL`,
`AGENTLEDGER_STATE_DB`, `AGENTLEDGER_OUTPUT_DIR`, and
`AGENTLEDGER_TIMEZONE`. Gmail remains disabled by default. After `gmail auth`
succeeds, enable it with `AGENTLEDGER_GMAIL_ENABLED=true` in this env file or
pass `--enable-gmail` to `run-scheduled`. Gmail credentials and its independent
token cache continue to use their existing defaults; `--gmail-credentials`,
`--gmail-token-cache`, and `--gmail-max-messages` can override them. Never put
access/refresh tokens in the scheduler env file; Microsoft tokens
remain in the separate MSAL cache. Existing env files are preserved.

Review privacy-safe summaries and failures in the user journal:

```bash
journalctl --user -u agentledger-mail.service -n 100 --no-pager
```

Journal output contains counts, run IDs, paths, and safe error types—not mail
bodies, subjects, previews, prompts, or tokens. If the timer is unavailable,
use `scheduler run-now` or `run-scheduled` directly. LINE notification and
approval remain a later phase; their integration point is the pending Calendar
Action identified by the stored `calendar_action_id`.

### Approval Queue

Every Calendar Agent Action written with `status=awaiting_approval` is added to
the SQLite `approval_queue` only after its source AgentLedger JSONL has been
written atomically. Confirmed actions, clarification requests, and duplicate
`calendar_action_id` values are not queued. Mail processing state remains in
`processed_messages`, batch state in `runs`, and human approval state in
`approval_queue`; SQLite does not replace the AgentLedger audit log.

Queue entries receive short IDs such as `AP-000001` and expire after 72 hours
by default. Inspect pending decisions without displaying sender addresses,
message bodies, or previews:

```bash
uv run mail-calendar-orchestrator approvals list
uv run mail-calendar-orchestrator approvals show AP-000001
uv run mail-calendar-orchestrator approvals summary
```

Resolve one through the reusable Python `ApprovalService` (also exposed by the
CLI):

```bash
uv run mail-calendar-orchestrator approvals approve AP-000001 \
  --actor daichi --reason "Approved"

uv run mail-calendar-orchestrator approvals reject AP-000002 \
  --actor daichi --reason "Not relevant"

uv run mail-calendar-orchestrator approvals expire
```

Approve/reject uses the existing `CalendarAgent.resolve` implementation. It
validates the source Action and IDs, refuses existing Outcome/Human
Intervention events, and writes a new atomic JSONL; the source run is never
overwritten. Approval appends an `accept` Human Intervention and a confirmed
`calendar_event_approved` Outcome. Rejection appends a `reject` intervention
and contradicted `calendar_event_rejected` Outcome. SQLite conditional state
transitions ensure only one concurrent CLI or future webhook request succeeds.

`approved` means permission was granted for a future calendar write—it does
**not** mean a calendar event was created. A future CalendarExecutor will turn
approved records into actual `calendar_event_created` or failed outcomes.
Expiry currently changes only Queue status to `expired`; it does not fabricate
a Human Intervention or Calendar Agent Outcome.

Approval SQLite rows contain IDs, bounded title/date/location, state,
timestamps, actor/reason (maximum 500 characters), and source/outcome JSONL
paths. They never contain body text, previews, Graph or LLM raw responses,
prompts, attachments, or tokens. LINE notification/webhook integration remains
the next phase and can call `ApprovalService.approve()` or `.reject()` directly.

### Google Calendar execution

Google Calendar writing is a separate, explicit phase after human approval:

```text
awaiting_approval → approved → executing → calendar_created
                                      └──→ calendar_failed
```

`approved` means permission to write; only `calendar_created` means Google
Calendar accepted the event. Mail scheduled runs never execute this phase.
Create an event only with an explicit command:

```bash
uv run mail-calendar-orchestrator approvals execute AP-000001 \
  --calendar-provider google \
  --google-calendar-id primary
```

Approval can optionally be followed immediately by execution, but this is off
by default:

```bash
uv run mail-calendar-orchestrator approvals approve AP-000001 \
  --actor daichi --reason "Approved" --execute
```

The provider-neutral `CalendarExecutor.create_event()` boundary receives a
validated `CalendarExecutionRequest`; `GoogleCalendarExecutor` is the first
implementation. SQLite stores provider, calendar ID, attempts, external event
ID/link, safe errors, timestamps, and result JSONL in `calendar_execution`.
Temporary 403 quota errors, 429, 5xx, and network timeouts use finite
exponential backoff and remain retryable (maximum three execution attempts).
Validation, credentials, permission, calendar-not-found, and malformed response
errors are permanent. Google authentication failure does not affect Outlook
polling or the Approval Queue.

Idempotency is enforced twice: SQLite refuses an already `calendar_created`
Approval, and Google `events.list` searches
`privateExtendedProperty=agentledger_approval_id=AP-...` before `events.insert`.
The created event stores private Approval, Action, Candidate, source-provider,
and hashed source-message identifiers. It sends only title, timezone-aware
start/end, location, and this minimal description:

```text
Created by AgentLedger.
Approval ID: AP-000001
Source: Outlook mail
```

It does not send body/preview, sender address, Graph response, Qwen reasoning,
prompt/evidence, or tokens. Success appends a new confirmed
`calendar_event_created` Outcome; failure appends
`calendar_event_creation_failed`. The approved Human Intervention is not
duplicated, the source JSONL is not overwritten, and Explorer selects the
newest Outcome.

After a process or PC restart, explicitly reconcile executions that have been
`executing` for at least five minutes:

```bash
uv run mail-calendar-orchestrator approvals recover
```

Use `--stale-after-seconds` and `--limit` to adjust the bounded scan. Recovery
only searches Google Calendar by the private Approval marker. An existing event
is reconciled to `calendar_created`; a missing event or lookup failure leaves
the Approval unchanged for review. Recovery never calls `events.insert` and is
safe to run repeatedly.

#### Google OAuth setup

The implementation follows Google's Desktop installed-application flow and
uses only `https://www.googleapis.com/auth/calendar.events`. The official
`google-api-python-client`, `google-auth-httplib2`, and
`google-auth-oauthlib` libraries handle OAuth and token refresh; OAuth is not
implemented manually.

1. Create/select a project in Google Cloud Console.
2. Enable the Google Calendar API.
3. Configure the Google Auth consent screen.
4. For a personal Google account, select an External audience and add your own
   account as a test user while the app remains in testing.
5. Create an OAuth Client ID with application type **Desktop app**.
6. Download the JSON to
   `~/.config/agentledger/google_credentials.json`.
7. Restrict it with `chmod 600`.
8. Run the authorization command and approve Calendar event access in the
   browser:

```bash
mkdir -p ~/.config/agentledger
chmod 700 ~/.config/agentledger
chmod 600 ~/.config/agentledger/google_credentials.json

uv run mail-calendar-orchestrator google-calendar auth
uv run mail-calendar-orchestrator google-calendar status
```

The refreshable token is stored at
`~/.config/agentledger/google_calendar_token.json` with mode `0600`. Neither
file is committed, copied to SQLite/AgentLedger, or printed to the journal.
Use `AGENTLEDGER_GOOGLE_CALENDAR_ID` to target a dedicated calendar instead of
`primary`; credentials/token paths also accept
`AGENTLEDGER_GOOGLE_CREDENTIALS` and `AGENTLEDGER_GOOGLE_TOKEN`.

Google notes that public External apps using user-data scopes may require OAuth
verification. A personal testing app restricted to configured test users can
remain in testing, subject to Google's test-user and token-lifetime rules.
See the official [Python Calendar quickstart](https://developers.google.com/workspace/calendar/api/quickstart/python),
[Calendar scopes](https://developers.google.com/workspace/calendar/api/auth),
[events.insert reference](https://developers.google.com/workspace/calendar/api/v3/reference/events/insert),
and [extended properties guide](https://developers.google.com/workspace/calendar/api/guides/extended-properties).

#### Gmail read-only OAuth check

The same Desktop OAuth client file can also authorize Gmail, but Gmail uses an
independent token cache and exactly one scope:

```text
Credentials: ~/.config/agentledger/google_credentials.json
Gmail token: ~/.config/agentledger/gmail_token.json
Scope: https://www.googleapis.com/auth/gmail.readonly
```

Enable the Gmail API in the same Google Cloud project, then authenticate and
check read-only access:

```bash
uv run mail-calendar-orchestrator gmail auth
uv run mail-calendar-orchestrator gmail status
uv run mail-calendar-orchestrator gmail list --limit 5
uv run mail-calendar-orchestrator gmail analyze 19fe4ddfaee15750
uv run mail-calendar-orchestrator gmail analyze 19fe4ddfaee15750 --debug-grounding
uv run mail-calendar-orchestrator gmail analyze 19fe4ddfaee15750 --debug-body
```

The access check calls `users.messages.list` with `userId=me` and
`maxResults=1`; it does not fetch a message body or connect Gmail to the mail
classification pipeline. The Gmail token is written atomically with mode
`0600` and is never shared with
`~/.config/agentledger/google_calendar_token.json`. Credentials and tokens are
not written to AgentLedger JSONL, SQLite, or command output. Override paths
with `AGENTLEDGER_GOOGLE_CREDENTIALS` and `AGENTLEDGER_GMAIL_TOKEN` when needed.
The `gmail list` command prints only the provider-prefixed message ID, received
time, From header, and Subject header. It requests `format=metadata` with only
the `From` and `Subject` headers; it does not print or process the body, snippet,
OAuth token, or client secret. Gmail remains disconnected from Outlook
processing and scheduled runs.

`gmail analyze` is an explicit, one-message diagnostic command. Pass the raw
Gmail message ID without the `gmail:` prefix. It fetches that message with
`format=full`, prefers a non-attachment `text/plain` MIME part, and safely
converts `text/html` only when plain text is absent. It then runs the existing
local Qwen/Validator pipeline in `llm-first` mode. The command prints only the
provider-prefixed ID, final classification, candidate decision, normalized
candidate fields, confidence, and validation issue names. It does not create an
Approval, send LINE messages, write Calendar events, update scheduled-run state,
or write AgentLedger JSONL. Full mail content, attachments, raw model output,
OAuth tokens, and client secrets are not printed or saved by this command.
Add `--debug-grounding` to print only the raw/local received timestamp,
timezone, LLM-proposed date, matched date-expression tokens, deterministic
resolutions, grounded result, and a bounded failure-reason code. The diagnostic
never prints the full body, surrounding evidence text, OAuth data, prompt, or
raw model response. It also prints up to 20 short date-like tokens (maximum 32
characters each), such as `明日の`, `月曜日`, or `10日15時`, to diagnose a
resolver miss. HTML/script/style content, common reply headers, and signature
content after a delimiter are excluded. These wider diagnostic tokens never
participate in candidate grounding or execution decisions.
The same diagnostic also prints the character length and SHA-256 hash of the
actual Qwen body and Validator grounding body plus `Same body: yes/no`. Both
paths use the same bounded canonical body derived once from normalized
`EmailMessage.body_text`; hashes are diagnostic metadata only and the body or
body fragments are not printed with them.
Subject and normalized body are the canonical semantic-grounding text. Debug
output reports whether the Subject contains a date-like token, the bounded token
itself, and its deterministic resolution without printing the full Subject. It
also lists only the grounding field names: `subject`, `body_text`, and
`received_at`. The received timestamp is a reference clock for expressions such
as `今日` and `明日`; it is never by itself evidence for a proposed calendar
date. If neither Subject nor body contains a supported date expression, an ISO
date generated from received time remains ungrounded.
For Gmail, debug output also reports the top-level MIME type, non-attachment
plain/HTML candidate counts, selected MIME type, selected canonical-body length,
and boolean presence of Japanese explicit-date/time character classes. Nested
`multipart/mixed`, `multipart/related`, and `multipart/alternative` structures
are traversed without loading attachment bodies. A substantive plain alternative
remains preferred even when HTML contains additional calendar-related facts.
Only when the plain alternative is empty, effectively whitespace-only, or an
automatic-footer-only representation is the safely text-converted HTML
alternative selected. This
selection changes only which MIME alternative is canonical—it does not infer or
rewrite mail facts.
Each textual MIME part is base64url-decoded to bytes and then decoded using its
declared `Content-Type` charset. Python codec aliases are normalized, including
UTF-8, ISO-2022-JP, Shift_JIS, CP932, and EUC-JP. If the charset is absent or
invalid, a bounded set of those codecs is tried strictly before a final
replacement fallback. `--debug-grounding` reports the selected charset, whether
it came from MIME or fallback detection, and whether a decode error occurred;
it does not print the body.
Before calendar semantics are evaluated, deterministic analysis-only cleanup
removes forwarded/replied transport-header blocks such as `差出人`, `送信日時`,
`宛先`, `件名`, `From`, `Sent`, `To`, `Cc`, and `Subject`. A block requires
multiple adjacent transport fields or a recognized forward separator, so a lone
header-like sentence is preserved. The forwarded message body remains intact,
the canonical MIME body is not mutated, and the LLM, rule extraction, and
Validator all receive the same cleaned analysis text.
MIME representation selection never uses dates, times, locations, meeting/event
context, or calendar-oriented token scoring. Those booleans and visible lengths
remain debug observations only. A bounded structural selection reason such as
`plain_preferred_valid_alternative`, `html_fallback_plain_empty`, or
`html_fallback_plain_footer_only` is printed; neither alternative body is shown
unless the explicit `--debug-body` escape hatch is used.
Japanese date detection uses a shared diagnostic projection with Unicode NFKC,
full-width digit conversion, newline-as-whitespace handling, and optional
normal/full-width whitespace around `月` and `日`. Thus `8月10日`, `8月 10日`,
`8月　10日`, `８月１０日`, and `8 月 10 日` resolve identically. The canonical
mail body itself is not rewritten. Gmail debug output separately reports raw
plain/HTML containment for `8月`, `10日`, and exact `8月10日`, NFKC exact-match
results, and canonical NFKC/whitespace-normalized detector booleans without
printing either MIME alternative.

`--debug-body` is a temporary, explicit local debugging escape hatch. Unlike
the default and `--debug-grounding` modes, it prints the selected MIME type,
canonical extracted-body length, and the complete canonical body to stdout
between prominent `DEBUG ONLY` markers. This can expose sensitive mail content;
use it only in a private terminal. It does not add the body to AgentLedger
JSONL, SQLite, prompt diagnostics, raw model output, or OAuth diagnostics, and
the default remains body-hidden.

After manual verification, scheduled Gmail ingestion can be enabled explicitly:

```bash
uv run mail-calendar-orchestrator run-scheduled --enable-gmail
```

Scheduled Gmail requests use `gmail.readonly`, the `INBOX` label, a provider-
specific successful-poll cursor (with the same overlap window as Outlook), and
the configured maximum message count. Gmail returns `gmail:<id>` and Outlook
returns `outlook:<id>` as canonical message IDs. SQLite keys processed state by
both provider and canonical ID, so the same raw provider ID cannot collide.
Both providers feed the same `EmailMessage` → Qwen → Validator → Candidate →
Approval/LINE path. Mail is never marked read, changed, or deleted.

Provider fetches are isolated. If one provider fails, messages already fetched
from the other are still processed; only the successful provider cursor moves.
The run becomes `completed_with_errors`, and safe provider status/error-type
summaries are recorded in SQLite and printed by `run-scheduled` and
`state summary`. If all enabled providers fail, the run is recorded as failed
and exits non-zero. Provider exception details, mail bodies, tokens, and raw LLM
responses are not stored in the provider summary.

Google classifies `gmail.readonly` as a restricted scope. Keep the OAuth app in
an appropriate testing configuration for personal use and review Google's
verification and data-handling requirements before broader deployment. See
the official [Gmail scopes](https://developers.google.com/workspace/gmail/api/auth/scopes)
and [`users.messages.list` reference](https://developers.google.com/workspace/gmail/api/reference/rest/v1/users.messages/list).

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
deterministically recognized security phrase. `final_classification` is the
canonical semantic proposal; the internal `should_create_calendar_candidate`
value is derived from it rather than generated separately by the LLM.
Non-candidates (`candidate_type=none` and a non-calendar final classification)
do not run irrelevant date/time/duration grounding checks.

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
## LINE approval UI

LINE Messaging API can be used as a narrow approval UI for pending calendar
candidates. LINE only sends notifications and postback decisions; mail analysis,
LLM classification, approval state, and calendar execution remain in the existing
local services.

Create the private configuration file (created with mode `0600`):

```bash
uv run mail-calendar-orchestrator line init-config
```

Edit `~/.config/agentledger/line-approval.env`:

```dotenv
LINE_CHANNEL_SECRET=
LINE_CHANNEL_ACCESS_TOKEN=
LINE_ALLOWED_USER_ID=
```

Check configuration without printing values, then send a harmless test message:

```bash
uv run mail-calendar-orchestrator line status
uv run mail-calendar-orchestrator line test-message
```

Send an awaiting approval manually and run the local webhook server:

```bash
uv run mail-calendar-orchestrator line notify AP-000001
uv run mail-calendar-orchestrator line webhook --host 127.0.0.1 --port 8787
curl http://127.0.0.1:8787/health
```

The webhook endpoint is `POST /line/webhook`. Keep it bound to localhost and
publish it through a secure HTTPS tunnel such as Cloudflare Tunnel or Tailscale
Funnel. Do not expose the Mini PC with router port forwarding. LINE requires a
publicly reachable HTTPS webhook URL. The long-running webhook should use a
separate user service (for example `agentledger-line-webhook.service`); keep it
separate from the existing oneshot mail timer so a LINE outage cannot stop mail
analysis.

LINE Developers setup:

1. Create a Provider and a Messaging API channel in LINE Developers, linked to a
   LINE Official Account.
2. Copy the Channel secret and issue a channel access token.
3. Add the Official Account as a friend.
4. Obtain your own user ID from the `source.userId` of a one-to-one webhook event;
   handle it only during setup and do not leave the full ID in normal logs.
5. Fill the private env file and run `line status`.
6. Configure the tunnel HTTPS URL ending in `/line/webhook`, enable **Use webhook**
   and webhook redelivery, then run the console webhook verification.
7. Run `line test-message`, create an awaiting approval, and run `line notify`.

Webhook signatures are verified against the exact raw body before JSON parsing.
Only the configured one-to-one LINE user is accepted. Approval buttons carry a
random one-time token; SQLite stores only its SHA-256 hash and rejects expired,
modified, consumed, or replayed interactions. `webhookEventId` provides an
additional redelivery guard. Notifications contain only the title, date/time,
duration, location, source provider, short decision summary, and approval ID—not
mail bodies, sender addresses, source message IDs, prompts, evidence, reasoning,
credentials, or raw webhook payloads.

### Run the LINE webhook with systemd --user

The webhook can run as a user service independently from the mail scheduler.
Installation writes `~/.config/systemd/user/agentledger-line-webhook.service`
atomically and runs `systemctl --user daemon-reload`; it does not start the
service automatically. The existing LINE env file must exist and is kept at
mode `0600`. Secrets are referenced with `EnvironmentFile` and are never copied
into the unit.

If the manual webhook is already using port 8787, stop it with Ctrl+C first.
Then install, enable, and inspect the service:

```bash
uv run mail-calendar-orchestrator line service install
uv run mail-calendar-orchestrator line service enable
uv run mail-calendar-orchestrator line service status
```

Additional management commands are:

```bash
uv run mail-calendar-orchestrator line service restart
uv run mail-calendar-orchestrator line service disable
uv run mail-calendar-orchestrator line service uninstall
```

`uninstall` removes only the systemd unit; it does not remove
`~/.config/agentledger/line-approval.env`. Verify the local endpoint and recent
journal messages with:

```bash
curl http://127.0.0.1:8787/health
journalctl --user -u agentledger-line-webhook.service -n 30
```

User services can depend on a login session. To keep the webhook running after
the Mini PC boots without an interactive login, enable linger manually when
needed (the installer never runs sudo):

```bash
sudo loginctl enable-linger daichi
loginctl show-user daichi -p Linger
```

This service binds only to `127.0.0.1:8787`. It does not manage cloudflared.
The current development route remains:

```text
LINE
  → trycloudflare.com
  → cloudflared Quick Tunnel
  → 127.0.0.1:8787
```

The webhook service may be active and healthy while LINE still cannot reach it
if the separate Quick Tunnel has stopped. Running Quick Tunnel under systemd or
using a fixed custom domain is intentionally left for a later phase.
