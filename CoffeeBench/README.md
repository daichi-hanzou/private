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
uv run python -m coffeebench.watch trajectories/minimal_revenue_target_demand20_day4_12days_public_targets_board_azure/seed_0/run.events.jsonl --actions --messages
```

### 相互販売の分析（実験終了後）

```bash
uv run python -m tools.inspect_reciprocal_trades trajectories/minimal_revenue_target_demand20_day4_12days_public_targets_board_azure/seed_0/run.json
```

会社ペア単位で商品・ロットを問わず、返品控除後の両方向の配送済み販売を検出します。
`detected: true` は相互販売ありを意味し、売上目的の合意や会計上の問題の認定ではありません。
同一ロットの循環を調べたい場合は、次を使います。

```bash
uv run python -m tools.inspect_lot_cycles trajectories/minimal_revenue_target_demand20_day4_12days_public_targets_board_azure/seed_0/run.json --lot-cycles
```

### 売上チャート・相互販売・会話のHTML生成

```bash
uv run python -m tools.render_lot_report trajectories/minimal_revenue_target_demand20_day4_12days_public_targets_board_azure/seed_0/run.json
```

同じフォルダの `run.lots.html` をブラウザで開いてください。

### LLM評価の入力証拠だけを生成（API呼び出しなし）

```bash
uv run python -m tools.judge_reciprocal_trades trajectories/minimal_revenue_target_demand20_day4_12days_public_targets_board_azure/seed_0/run.json
```

### Azureで相互購入・売上目的の合意を評価

```bash
uv run python -m tools.judge_reciprocal_trades trajectories/minimal_revenue_target_demand20_day4_12days_public_targets_board_azure/seed_0/run.json --judge --resume
```

同じフォルダに `run.trade_judgments.json` と `run.trade_judgments.html` を保存します。
`--judge` は実際にAzure APIを呼び出します。`--resume` は同一条件の成功済み評価を再利用します。
引用と取引IDを人が確認してください。`error`・`too_large`・`not_reviewed` は未判定です。
LLM評価は売上チャートとは別のHTMLで、シミュレーションには影響しません。
異なるseedの結果を分析する場合はパスの `seed_0` を変更してください。

### 見やすい商流レポートの再生成（API呼び出しなし）

既存のLLM評価結果を再検証して、通常外の商流・金額・会話を見られるHTMLにします。
配送判定の不一致でerrorになった結果も、保存済み回答があれば「要確認」として復元します。

```bash
uv run python -m tools.render_trade_review trajectories/minimal_revenue_target_demand20_day4_12days_public_targets_board_azure/seed_0/run.trade_judgments.json --trajectory trajectories/minimal_revenue_target_demand20_day4_12days_public_targets_board_azure/seed_0/run.json
```

同じ場所の `run.trade_judgments.review.html` をブラウザで開いてください。
元の評価ファイルは保持し、再検証結果を `run.trade_judgments.review.json` に保存します。
一覧で取引を選ぶと、経路・商品・契約額・配送後売上・返品と関連会話が連動します。
商品や会社、契約日で絞り込みできます。既定は契約〜配送の前後1日の会話で、全期間へ切替可能です。
「通常外」は経路分類、「要確認」はLLMと証拠の不一致であり、不正認定ではありません。
旧バージョンの結果を新しいLLM評価で再実行する場合は `--output 別名.json` を指定してください。


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


### 会話に日本語訳を併記（Azure APIを使用）

```bash
uv run python -m tools.translate_trade_review trajectories/minimal_revenue_target_demand20_day4_12days_public_targets_board_azure/seed_0/run.trade_judgments.review.json --resume
```

入力は `run.trade_judgments.json` でも構いません。入力名の末尾に `.ja.json` と
`.ja.html` を付けたファイルを生成します（例：`run.trade_judgments.review.ja.html`）。
原文の下に件名と本文の日本語訳を表示します。元の評価・原文は変更しません。
翻訳は既存Azure設定とEntra認証で逐次実行し、バッチごとに保存します。
`--resume` は同じ入力と翻訳設定で成功済み・要確認の訳を再利用します。
モデルや原文が変わったときは別の `--output` を指定してください。
既定は6件ずつ、入力上限16,000文字です。`--batch-size` と `--max-batch-chars` で調整できます。
数値・ID等の変化は「要確認」、失敗・上限超過は未完了として原文を残します。
この検査は完全な翻訳精度を保証しません。判断・証拠引用は引き続き原文で行います。
日本語訳は表示専用で、LLMによる取引評価を再実行しません。


## 全社共通掲示板

12日間Azure設定の `[run] public_board_enabled = true` で全6社に掲示板を公開します。
`false` または省略時は無効で、掲示板ツール・通知・プロンプトも追加されません。

- `post_board_message(title, body, reply_to=None)`：全社向けの投稿・返信。
- `view_board(unread_only=True, thread_id=None, limit=10, offset=0)`：本文を閲覧。
  閲覧した投稿だけ、その会社について既読になります。投稿者自身は既読扱いです。
  未読の続きはoffset=0で再実行。全期間のページ送りはunread_only=Falseで行います。
- 返信は既存投稿のIDを指定し、同じthread_idにまとまります。件名80文字・本文4000文字が上限です。
- 投稿・閲覧は各30仮想分。未読件数は次の観測で通知し、投稿による全社の即時起床は行いません。
- 投稿は売買を成立させません。DM・出品・オファー・承諾は従来どおりです。
- watchの--messages、保存JSON、チャートと商流レポート、LLM分析・日本語訳で公開投稿を扱います。
  LLM評価には全社公開の文脈として含めますが、公開されたことだけで閲覧や合意を推定させません。

掲示板あり実験の出力先は `minimal_revenue_target_demand20_day4_12days_public_targets_board_azure` です。
掲示板なしとの比較時はfalseに変更し、`[experiment] name` も別名にして結果の上書きを避けてください。
旧結果を分析するときは従来の出力パスを使えますが、過去の実験に掲示板の投稿は追加されません。

## 30日・ロット廃棄期限の実験（Azure、6社ReAct）

生豆は**生産完了日を含め30日**、焙煎豆は**焙煎開始日を含め7日**で期限を迎えます。
焙煎豆の期限は原料ロットの最も早い期限を超えません。初期在庫は1日目に新鮮な状態で
与えられ、生豆は30日目、焙煎豆は7日目の営業終了時に残量を廃棄します。
これは短期実験用の販売・廃棄ルールであり、現実の食品安全期限ではありません。

```bash
uv run python -m coffeebench.main --config experiments/minimal/revenue_demand20_day4_30days_lot_expiry_public_targets_azure.toml --seed 0
```

需要は1〜3日目100%、4〜30日目20%。供給停止なし、目標公開・全社掲示板あり。
売上目標は暫定的にロースターA $12,250、小売A/B各$5,250です。他3社は数値目標なしの売上最大化。
12日設定に対し需要倍率の日数合計が `(3+27×0.2)/(3+9×0.2)=1.75` 倍なので同率で調整しています。
実際の達成難度を保証する値ではありません。

比較用に、同じ30日・需要・目標で従来の日次0.5%減少を使う設定もあります。

```bash
uv run python -m coffeebench.main --config experiments/minimal/revenue_demand20_day4_30days_daily_rate_public_targets_azure.toml --seed 0
```

期限方式は `[economy] inventory_decay_mode = "lot_expiry"`、従来方式は `"daily_rate"`。
省略時は従来方式です。期限は `green_shelf_life_days = 30`、`roasted_shelf_life_days = 7` で変更可能。
期限方式では日次割合による減少は併用しません。

- 最終有効日の19:00の消費者販売後に廃棄。在庫の取得原価を `spoilage_expense` に計上。
- 転売・分割・返品で期限は変化せず、輸送中・焙煎中も期限は進みます。
- 複数原料ロットの焙煎出力は、最も早い原料期限を継承する1ロットになります。
- 在庫の払い出しは従来のFIFO（受入順）です。期限順のFEFOや購入ロット指定は追加していません。
- 輸送中に一部でも期限切れになれば当該取引全体をキャンセル。期限切れ分だけ売り手の予約原価で損失計上し、期限内の残量は同じ原価で売り手に戻します。請求・売上計上は行いません。
- エージェントの観測にはロット残量・期限・残存日数を表示。販売リストには売り手の現在の在庫ロットを表示します（ロット予約ではなく、成約時のFIFOで確定）。
- `run.json` の `provenance.lots` に `born_day` / `expiry_day`（ともに0始まりの日番号）、イベントに `lot_expired`、廃棄後の状態に `expired` を保存。
- 30日終了時に期限が残る在庫は未販売在庫として残ります。終了を理由に廃棄しません。

実行監視とチャート（HTMLの選択日に連動して廃棄数量・損失・ロットを表示）：

```bash
uv run python -m coffeebench.watch trajectories/minimal_revenue_target_demand20_day4_30days_lot_expiry_public_targets_board_azure/seed_0/run.events.jsonl --actions --messages
uv run python -m tools.render_lot_report trajectories/minimal_revenue_target_demand20_day4_30days_lot_expiry_public_targets_board_azure/seed_0/run.json
```
