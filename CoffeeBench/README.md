<div align="center" style="line-height: 1;">
<h1>CoffeeBench: Benchmarking Long-Horizon LLM Agents in Heterogeneous Multi-Agent Economies</h1>


  |
  <a href="https://arxiv.org/abs/2606.16613" target="_blank">📄 Paper</a>
  &nbsp;|
  <a href="https://pub.sakana.ai/coffeebench/index.html" target="_blank">📝 Blog</a>
  &nbsp;|
    <a href="https://pub.sakana.ai/coffeebench/trajectories.html" target="_blank">🔍 Trajectories</a>
  &nbsp;|

  <br/>

<img src="./assets/cumulative_net_income.gif" width="80%"/>
</div>


## Azure・6社ReAct実験のコマンド

このブランチの実験・分析コマンドです。以下はすべてリポジトリ内の
`CoffeeBench` ディレクトリで実行してください。詳細は
[実験設定と分析の説明](MINIMAL_EXPERIMENT.md)を参照してください。

依存関係は `uv sync --locked` で同期します。Azure実行時は `.env` に
`AZURE_OPENAI_ENDPOINT`（リソースのルートURL）と `AZURE_OPENAI_DEPLOYMENT` を設定し、
`az login` 等で `DefaultAzureCredential` が利用できる認証を準備してください。
Azure APIキー・モデル名称・日付付きAPIバージョンは不要です。

### 実験開始（12日間・相互目標公開・4日目以降需要20％）

```bash
uv run python -m coffeebench.main --config experiments/minimal/revenue_demand20_day4_12days_public_targets_azure.toml --seed 0
```

### 実行中のアクション・会話の監視（別ターミナル）

```bash
uv run python -m coffeebench.watch trajectories/minimal_revenue_target_demand20_day4_12days_public_targets_azure/seed_0/run.events.jsonl --actions --messages
```

### 相互販売の分析（実験終了後）

```bash
uv run python -m tools.inspect_reciprocal_trades trajectories/minimal_revenue_target_demand20_day4_12days_public_targets_azure/seed_0/run.json
```

会社ペア単位で商品・ロットを問わず、返品控除後の両方向の配送済み販売を検出します。
`detected: true` は相互販売ありを意味し、売上目的の合意や会計上の問題の認定ではありません。
同一ロットの循環を調べたい場合は、次を使います。

```bash
uv run python -m tools.inspect_lot_cycles trajectories/minimal_revenue_target_demand20_day4_12days_public_targets_azure/seed_0/run.json --lot-cycles
```

### 売上チャート・相互販売・会話のHTML生成

```bash
uv run python -m tools.render_lot_report trajectories/minimal_revenue_target_demand20_day4_12days_public_targets_azure/seed_0/run.json
```

同じフォルダの `run.lots.html` をブラウザで開いてください。

### LLM評価の入力証拠だけを生成（API呼び出しなし）

```bash
uv run python -m tools.judge_reciprocal_trades trajectories/minimal_revenue_target_demand20_day4_12days_public_targets_azure/seed_0/run.json
```

### Azureで相互購入・売上目的の合意を評価

```bash
uv run python -m tools.judge_reciprocal_trades trajectories/minimal_revenue_target_demand20_day4_12days_public_targets_azure/seed_0/run.json --judge --resume
```

同じフォルダに `run.trade_judgments.json` と `run.trade_judgments.html` を保存します。
`--judge` は実際にAzure APIを呼び出します。`--resume` は同一条件の成功済み評価を再利用します。
引用と取引IDを人が確認してください。`error`・`too_large`・`not_reviewed` は未判定です。
LLM評価は売上チャートとは別のHTMLで、シミュレーションには影響しません。
異なるseedの結果を分析する場合はパスの `seed_0` を変更してください。

---

## Overview

CoffeeBench is a benchmark for evaluating how much net income an LLM agent can generate as a coffee roaster over 90 days in a multi-agent economy with two farmers, two roasters, and two retailers.

<figure>
  <img src="./assets/CoffeeBench.svg" alt="Overview of CoffeeBench" style="width: 100%">
  <figcaption>Overview of CoffeeBench.</figcaption>
</figure>


## How to run

Install dependencies:
```bash
uv sync
```

Create `.env` and set your model-provider API keys (see `.env.example`):

```
OPENAI_API_KEY="sk-..."
ANTHROPIC_API_KEY="sk-..."
GEMINI_API_KEY="AI..."
OPENROUTER_API_KEY="sk-..."
```

Run a single simulation:

> NOTE: it takes over $200 in API costs and over 5 hours to run.
```bash
# Single 90-day run (Sonnet driving roaster_A; all other firms on Sonnet).
uv run python -m coffeebench.main \
    --config experiments/roaster_focal_sonnet.toml --seed 0
```




Monitor the simulation in real time with the web dashboard:
```bash
# Live web dashboard.
uv run streamlit run coffeebench/web.py
```

<figure>
  <img src="./assets/viewer.png" alt="Viewer of CoffeeBench" style="width: 100%">
  <figcaption>Screen shot of the web viewer.</figcaption>
</figure>


Run the full battery of experiments:

```bash
# Sweep the production matrix: 5 focal models × 3 seeds.
for tag in haiku sonnet opus gpt gemini; do
  for seed in 0 1 2; do
    uv run python -m coffeebench.main \
        --config experiments/roaster_focal_${tag}.toml --seed ${seed}
  done
done
```

## Citation
If you find our work interesting, please consider citing our paper:
```bibtex
@misc{sugiura2026coffeebenchbenchmarkinglonghorizonllm,
      title={CoffeeBench: Benchmarking Long-Horizon LLM Agents in Heterogeneous Multi-Agent Economies},
      author={Issa Sugiura and Daichi Hattori and Kazuo Araragi and Keita Ogawa and Shota Onose and Taro Makino and Teppei Usuki and Takashi Ishida},
      year={2026},
      eprint={2606.16613},
      archivePrefix={arXiv},
      primaryClass={cs.AI},
      url={https://arxiv.org/abs/2606.16613},
}
```
