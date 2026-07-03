## Mini Coffee ORS example

This repo now includes a minimal CoffeeBench-style ORS environment:

- `mini_coffee_env.py`
- `test_mini_coffee_client.py`
- `run_mini_coffee_sim.py`
- `mini_coffee_agent_runner.py`

It keeps only the core loop:

- buy inventory
- set retail prices
- advance one day
- receive sales and holding-cost results
- finish the episode and get a reward

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
