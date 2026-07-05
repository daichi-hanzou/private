## Mini Coffee ORS example

This repo now includes a minimal CoffeeBench-style ORS environment with trader inventory, ledger-derived credit metrics, and direct forward contracts:

- `mini_coffee_env.py`
- `test_mini_coffee_client.py`
- `run_mini_coffee_sim.py`
- `mini_coffee_agent_runner.py`
- `compare_mini_coffee_strategies.py`

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

To compare simple baseline strategies:

```bash
uv run python compare_mini_coffee_strategies.py
```
