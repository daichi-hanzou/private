# Mini Coffee 30-Day Calibration Config

Created: 2026-07-08

This file is the source of truth for the 30-day v2 robot calibration. The
original two LLM smoke runs are retained only as old-world records.

## Code Version

- Git commit: `734042781672ada2c9dbe998812eca4d167fbb14`
- Branch at capture time: `mini_coffee_bench_push_v3`
- v2 roster code status at freeze time: based on the commit above, with local
  roster edits in `mini_coffee_env.py` and regression tests in
  `test_mini_coffee_env.py`. Once committed, use that commit hash for LLM smoke
  reruns.
- Relevant files:
  - `mini_coffee_env.py`
  - `gate_check_bio.py`

## V2 Frozen Calibration

Created: 2026-07-08

The v2 roster intentionally breaks the public-price shortcut while preserving
spot prices, harvest schedules, and financial parameters:

  gem.
  cost, or BasePolicy parameters were changed for v2.

Official 30-day robot scale, 100 seeds:

| Policy | Mean profit |
|---|---:|
| `blind` | `889` |
| `blind_expensive` | `994` |
| `investigator` | `1116` |
| `oracle` | `1235` |

Derived checks:


The file `gate_check_results_30d_v2.csv` is the official v2 calibration output.
Do not score new LLM runs against the old 20-day CSV or the v1 30-day smoke CSV.

## V3 Demand Spikes

Created: 2026-07-09

v3 changes the world by adding occasional festival demand spikes and imprecise
market bulletins. The v2 robot scale and any v2 LLM runs are old-world records
and must not be used to score v3 runs. Keep all v2 CSVs and logs for audit, but
rerun robot calibration before evaluating LLM behavior in v3.

Demand spike parameters:

| Setting | Value |
|---|---:|
| Spike count per 30-day episode | `2` |
| Spike day range | `Day 8-Day 27` |
| Minimum distance between spikes | `5 days` |
| Spike demand multiplier | `2.2` |
| Forecast lead days | `3 days` and `1 day` before a forecasted spike |
| Forecast true-positive rate | `80%` per spike |
| False-positive bulletins | `1` per episode |
| Bulletin text | `Market bulletin: A local festival may lift coffee demand within the next 3 days.` |

The prompt body now tells LLMs that demand is usually stable but occasional
festival days can multiply demand, and that market bulletins may give imprecise
advance notice. `STRATEGY_HINT` remains frozen and unchanged.

Trader purchase logs now include forecast context fields for post-hoc behavior
analysis: `inventory_at_purchase`, `active_forecast`, `days_to_next_spike`, and
`after_shortfall_event`. `proactive_purchase` and `reactive_purchase` are derived
labels for analysis only and do not affect environment behavior.

Robot calibration for v3 uses five policies:

| Policy | Role |
|---|---|
| `blind` | Existing low-price baseline, unchanged |
| `blind_expensive` | Existing high-price public-rule check, unchanged |
| `investigator` | Existing paid-information baseline, unchanged |
| `oracle` | Existing true-reliability upper bound, unchanged |
| `planner` | New Oracle-derived policy that reacts to market bulletins by pre-stocking trader inventory |

v3 gate command:

```powershell
$env:MINI_COFFEE_DEBUG_TOOLS = "1"
$env:MINI_COFFEE_TOTAL_DAYS = "30"
$env:MINI_COFFEE_DISABLE_INCENTIVE = "1"
$env:ORS_PORT = "8093"
uv run python mini_coffee_env.py
```

```powershell
$env:MINI_COFFEE_ORS_URL = "http://localhost:8093"
$env:MINI_COFFEE_TOTAL_DAYS = "30"
uv run python gate_check_bio.py --seeds 100 --total-days 30 --csv gate_check_results_30d_v3.csv
```

Official v3 robot scale: to be filled after `gate_check_results_30d_v3.csv` is
generated and reviewed.

## Strategy Hint (frozen)

The following hint is sent to every LLM run as part of the initial messages
(system message for Azure OpenAI, second user message for Anthropic).
It is part of the world definition for v2 experiments. Changing this text
invalidates comparisons and requires rerunning robot calibration and all LLM runs.

> Balance cheap but risky direct procurement against immediate trader inventory.
> Use investigations sparingly and consider forward contracts when future supply
> matters. On the first turn, avoid waiting without action. Before any tool call
> on every turn, first write a brief 1-3 sentence decision note that explains
> your current view of inventory, supply risk, and the next action you will take.

Note: "Use investigations sparingly" is a behavioral nudge on investigation
frequency. It is held constant across all models and seeds; model differences
in investigation usage must be interpreted relative to this shared hint.

## World Configuration

| Setting | Value | Evidence |
|---|---:|---|
| `MINI_COFFEE_TOTAL_DAYS` | `30` | Both agent logs record `total_days: 30`; prompts say "30 days". |
| `MINI_COFFEE_DISABLE_INCENTIVE` | `1` | Used for 30-day calibration to match the intended LLM smoke launch procedure. The agent logs do not persist this environment bit directly. |
| `MINI_COFFEE_DEBUG_TOOLS` for LLM smoke | `0` | Agent prompts/tool schemas contain no `debug_*` tools. |
| `MINI_COFFEE_DEBUG_TOOLS` for robot calibration | `1` | Required by `gate_check_bio.py` for `debug_set_seed` and `debug_get_true_state`. |
| `INVESTIGATION_COST` | `10.0` | `mini_coffee_env.py`; LLM logs show investigate events with `cost: 10.0`. |
| `MINI_COFFEE_WARM_START_DAYS` | `90` | Default in `mini_coffee_env.py`; prompt text exposes 90-day fulfillment history. |

## Legacy LLM Smoke Runs

| Agent log | Env log | Seed | Profit | Investigation spend | Seed evidence |
|---|---|---:|---:|---:|---|
| `tmp/agent_20260708_100421_gpt-5.2.jsonl` | `tmp/mini_coffee_1ea6d306.jsonl` | `0` | `928.09` | `10.0` | Replayed seed 0 gives `sunrock` metrics `delivery_rate=0.40`, `sample_count=20`, matching the first investigation in the env log. |
| `tmp/agent_20260708_102215_gpt-5.2.jsonl` | `tmp/mini_coffee_722ba189.jsonl` | `1` | `577.30` | `30.0` | Replayed seed 1 gives `sunrock` metrics `delivery_rate=0.64`, `sample_count=25`, matching the first investigation in the env log. |

Both LLM smoke prompts exposed `investigate_farmer`, and both runs used at
least one investigation. Neither prompt exposed `debug_set_seed` nor
`debug_get_true_state`.

These two smoke runs were produced before the v2 roster fix and are excluded
from official scoring. Rerun the two-seed LLM smoke on the v2 roster before
using the v2 robot scale.

## Robot Calibration Command

Server:

```powershell
$env:MINI_COFFEE_DEBUG_TOOLS = "1"
$env:MINI_COFFEE_TOTAL_DAYS = "30"
$env:MINI_COFFEE_DISABLE_INCENTIVE = "1"
$env:ORS_PORT = "8093"
uv run python mini_coffee_env.py
```

Gate:

```powershell
$env:MINI_COFFEE_ORS_URL = "http://localhost:8093"
$env:MINI_COFFEE_TOTAL_DAYS = "30"
uv run python gate_check_bio.py --seeds 100 --total-days 30 --csv gate_check_results_30d_v2.csv
```

The existing 20-day `gate_check_results.csv` must not be overwritten.

## V2 LLM Smoke Command

Run after committing the v2 roster fix, with the checked-out git hash matching
the v2 code used for calibration.

Server:

```powershell
$env:MINI_COFFEE_DEBUG_TOOLS = "0"
$env:MINI_COFFEE_TOTAL_DAYS = "30"
$env:MINI_COFFEE_DISABLE_INCENTIVE = "1"
$env:MINI_COFFEE_RUN_SEED = "0"
$env:ORS_PORT = "8093"
uv run python mini_coffee_env.py
```

Use seed `0` for the first smoke run and seed `1` for the second smoke run.
Keep debug tools disabled for both runs.

Runner (separate shell from the server; the runner reads its own copy of
`MINI_COFFEE_TOTAL_DAYS`, guarded by the prompt-horizon check):

```powershell
$env:MINI_COFFEE_ORS_URL = "http://localhost:8093"
$env:MINI_COFFEE_TOTAL_DAYS = "30"
$env:MINI_COFFEE_LLM_PROVIDER = "azure_openai"
$env:AZURE_OPENAI_ENDPOINT = "<endpoint>"
$env:AZURE_OPENAI_DEPLOYMENT = "gpt-5.2"
# Auth: set one of AZURE_OPENAI_API_KEY / AZURE_OPENAI_TOKEN /
# AZURE_OPENAI_USE_DEFAULT_CREDENTIAL=true
uv run python mini_coffee_agent_runner.py
```

If the runner and the environment disagree on the horizon, the runner exits
with "Prompt horizon mismatch" before any LLM call is made.

## Farmer Roster

`harbor_roast_supply` is a farmer profile, not the trader desk. `andes_mist`
is also a farmer profile. Both names were introduced in git commit `781c399`
and are part of the current 30-day world. Because the
current roster differs from the older 20-day calibration reference, the 20-day
values are treated as old-world records and are not used for 30-day scoring.

| Farmer ID | Display name | Reliability | Standard spot range | Premium spot range |
|---|---|---:|---:|---:|
| `sierra_verde` | Sierra Verde Cooperative | `0.95` | `$4.00-$5.00` | `$7.00-$8.20` |
| `andes_mist` | Andes Mist Collective | `0.55` | `$4.20-$5.10` | `$6.80-$8.30` |
| `cloud_peak` | Cloud Peak Estate | `0.55` | `$3.80-$4.80` | `$6.20-$7.50` |
| `cedar_valley` | Cedar Valley Farm | `0.80` | `$3.90-$4.70` | `$7.40-$8.40` |
| `riverbend` | Riverbend Growers | `0.80` | `$3.40-$4.40` | `$6.00-$7.00` |
| `sunrock` | Sunrock Producers | `0.55` | `$3.20-$4.00` | `$5.80-$6.80` |
| `harbor_roast_supply` | Harbor Roast Supply | `0.95` | `$4.70-$5.40` | `$7.90-$9.00` |
| `loma_dorada` | Loma Dorada Estate | `0.55` | `$4.50-$5.20` | `$6.50-$7.70` |
| `norte_azul` | Norte Azul Cooperative | `0.95` | `$4.10-$4.80` | `$6.90-$7.90` |
| `rainforest_direct` | Rainforest Direct | `0.92` | `$3.60-$4.30` | `$6.40-$7.30` |
## V3.4 Festival Logistics Freeze

Created: 2026-07-14

v3.4 introduces a logistics channel freeze during festival days: farm-origin deliveries are suspended for the duration of the festival while trader transfers remain immediate. This creates a short-term delivery dead zone that increases the value of proactive trader prestocking and planning. Key changes:

- `FESTIVAL_LOGISTICS_FREEZE = True`
- Farmer deliveries scheduled on festival days are deferred until after the festival rather than partially refunded immediately.
- Planner uses a conservative farmer overbook factor (`1.0`) while forecasts are active and must rely on trader prestock for guaranteed inventory.

Official v3.4 robot scale, 100 seeds:

| Policy | Mean profit |
|---|---:|
| `blind` | `894.5` |
| `blind_expensive` | `1087.3` |
| `investigator` | `1183.7` |
| `oracle` | `1313.5` |
| `planner` | `1374.3` |

Derived checks:

- Oracle - Blind gap: `419.0`
- `blind_expensive` position: `(1087.3 - 894.5) / (1313.5 - 894.5) = 46.3%`
- Investigator capture: `(1183.7 - 894.5) / (1313.5 - 894.5) = 69.0%`
- Planner > Oracle wins: `70%` of seeds
- Gate status: GATE1 pass (gap 46.8%), GATE2 pass (capture 69.0%), GATE3 pass (Planner outperforms Oracle), `blind_expensive` pass.

The file `gate_check_results_30d_v3_4.csv` is the official v3.4 calibration output.

Run the v3.4 calibration with:

```powershell
$env:MINI_COFFEE_ORS_URL = "http://localhost:8093"
$env:MINI_COFFEE_TOTAL_DAYS = "30"
uv run python gate_check_bio.py --seeds 100 --total-days 30 --csv gate_check_results_30d_v3_4.csv
```

Keep prior CSVs for audit. All LLM runs scored against this v3.4 scale use Planner as the target policy baseline (1374.3 profit).

