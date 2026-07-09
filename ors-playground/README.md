## Mini Coffee ORS example

This repo now includes a minimal CoffeeBench-style ORS environment with trader inventory, ledger-derived credit metrics, and direct forward contracts:

- `mini_coffee_env.py`
- `test_mini_coffee_client.py`
- `run_mini_coffee_sim.py`
- `mini_coffee_agent_runner.py`
- `compare_mini_coffee_strategies.py`
- `mini_coffee_event_logger.py`
- `render_mini_coffee_charts.py`

It keeps a small but useful supply-chain loop:

- inspect direct farm offers
- investigate farmer fulfillment and liquidity metrics
- choose spot direct, forward direct, or trader procurement
- set retail prices
- advance one day to harvest, fulfill contracts, and sell demand
- receive sales, holding-cost, and contract-fulfillment results
- finish the episode and get a reward

Observation model:

- visible to the agent: current retail state, current farm spot offers, current trader stock and price, investigated farmer metrics, open direct contracts
- hidden from the agent: future harvest schedules, future trader inbound deliveries, internal fulfillment multipliers, contract-break incentives

Run the server:

```bash
uv run python mini_coffee_env.py
```

In another terminal, run the client:

```bash
uv run python test_mini_coffee_client.py
```

To run a full 7-day simulation with a fixed policy:

```bash
uv run python run_mini_coffee_sim.py
```

To let an LLM play the environment:

```bash
uv run python mini_coffee_agent_runner.py
```

Provider selection:

- Anthropic via `.env`:

Create or edit `ors-playground/.env`:

```bash
MINI_COFFEE_LLM_PROVIDER=anthropic
ANTHROPIC_API_KEY=...
ANTHROPIC_MODEL=claude-opus-4-8
MINI_COFFEE_ORS_URL=http://localhost:8093
MINI_COFFEE_TOTAL_DAYS=30
MINI_COFFEE_DISABLE_INCENTIVE=1
MINI_COFFEE_DEBUG_TOOLS=0
```

Then run the agent:

```bash
uv run python mini_coffee_agent_runner.py
```

- Anthropic via shell variables:

```bash
ANTHROPIC_API_KEY=... \
ANTHROPIC_MODEL=claude-opus-4-8 \
uv run python mini_coffee_agent_runner.py
```

- OpenAI GPT:

```bash
MINI_COFFEE_LLM_PROVIDER=openai \
OPENAI_API_KEY=... \
OPENAI_MODEL=gpt-5 \
uv run python mini_coffee_agent_runner.py
```

- Azure OpenAI:

```bash
MINI_COFFEE_LLM_PROVIDER=azure_openai \
AZURE_OPENAI_ENDPOINT=... \
AZURE_OPENAI_DEPLOYMENT=... \
AZURE_OPENAI_API_KEY=... \
AZURE_OPENAI_API_VERSION=2024-10-21 \
uv run python mini_coffee_agent_runner.py
```

If you use a bearer token instead of an API key, set `AZURE_OPENAI_TOKEN` instead of `AZURE_OPENAI_API_KEY`.

To compare simple baseline strategies:

```bash
uv run python compare_mini_coffee_strategies.py
```

To generate a CoffeeBench-style replay log and static charts:

```bash
MINI_COFFEE_ORS_URL=http://localhost:8086 uv run python run_mini_coffee_sim.py
uv run python render_mini_coffee_charts.py
```

The simulation now writes one JSONL log per run under:

```bash
ors-playground/workspace/output/mini_coffee_runs/
```

`render_mini_coffee_charts.py` reads the latest JSONL by default and emits a static HTML dashboard next to it.
