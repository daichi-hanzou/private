# Vending-Bench

LLMエージェントの長期タスク遂行能力を評価するためのベンチマーク環境。自動販売機ビジネスのシミュレーションを通じて、エージェントの計画能力、記憶管理、適応的意思決定を評価します。

## 概要

Vending-Benchは、LLMエージェントが自動販売機事業者として以下のタスクを遂行する能力を測定します：

- **仕入先との交渉**: メール形式で商品を発注
- **在庫管理**: 倉庫から自動販売機への商品補充
- **価格設定**: 需要に応じた価格戦略
- **売上最適化**: 長期的な純資産の最大化

## 特徴

- **2000メッセージロールアウト**: 最大2000ターンまでシミュレーション実行
- **価格弾力性モデル**: 現実的な顧客購買行動のシミュレーション
- **コンテキスト管理**: 30,000トークン制限下でのメモリ管理
- **完全オフライン実行**: 固定データモードでLLM API不要

## インストール

```bash
cd vending_bench
pip install -e .
```

開発用依存関係を含める場合:

```bash
pip install -e ".[dev]"
```

## クイックスタート

### 基本実行

```bash
# デフォルト設定で実行
python -m vending_bench run

# 設定ファイルを指定
python -m vending_bench run --config configs/default.yaml

# 最大日数を指定
python -m vending_bench run --max-days 30

# シード値を指定（再現性のため）
python -m vending_bench run --seed 42
```

### その他のコマンド

```bash
# 設定の検証
python -m vending_bench validate-config configs/default.yaml

# シミュレーション情報の表示
python -m vending_bench info
```

## 設定

### 設定ファイル例 (YAML)

```yaml
environment:
  initial_balance: 500.0      # 初期資金 ($)
  daily_fee: 2.0              # 日次運営費 ($)
  max_messages: 2000          # 最大メッセージ数
  max_days: 365               # 最大シミュレーション日数
  machine_rows: 4             # 自販機の行数
  machine_columns: 3          # 自販機の列数
  slot_capacity: 10           # 各スロットの容量
  start_date: "2024-03-01"    # シミュレーション開始日

agent:
  context_tokens: 30000       # コンテキストトークン上限
  memory_enabled: true        # メモリツール有効化

demand:
  seed: 42                    # 乱数シード（再現性）
  optimal_variety: 6          # 最適商品種類数
  variety_penalty_max: 0.5    # 多品種ペナルティ上限
  noise_std: 0.1              # 需要ノイズ標準偏差

  # 曜日別売上係数 (月〜日)
  weekday_multipliers: [0.8, 0.85, 0.9, 0.95, 1.1, 1.3, 1.2]

  # 月別売上係数 (1〜12月)
  month_multipliers: [0.7, 0.75, 0.85, 0.95, 1.0, 1.15, 1.2, 1.15, 1.0, 0.95, 0.85, 0.9]

  # 天候別売上係数
  weather_multipliers:
    sunny: 1.1
    cloudy: 1.0
    rainy: 0.85

supplier:
  delivery_days_min: 2        # 最短配送日数
  delivery_days_max: 5        # 最長配送日数

llm:
  provider: "openai"          # LLMプロバイダー
  model: "gpt-4o"             # モデル名

embedding:
  provider: "tfidf"           # 埋め込みプロバイダー (tfidf/openai)
  model: null                 # OpenAI使用時のモデル名

logging:
  trace_dir: "logs/traces"    # トレースログ出力先
  metrics_dir: "logs/metrics" # メトリクスログ出力先
  log_level: "INFO"           # ログレベル
```

## 設計方針

### アーキテクチャ

```
vending_bench/
├── environment/       # 環境状態管理
│   ├── vending_machine.py  # 自販機モデル
│   ├── storage.py          # 倉庫（在庫）管理
│   ├── account.py          # 口座・財務管理
│   ├── clock.py            # シミュレーション時間
│   └── state.py            # 環境状態の統合
├── simulation/        # シミュレーション
│   ├── demand_model.py     # 価格弾力性による需要モデル
│   ├── supplier.py         # 仕入先シミュレーション
│   └── email_system.py     # メールシステム
├── tools/             # エージェントツール
│   ├── base.py             # ツール基底クラス
│   ├── main_agent_tools.py # メインエージェント用
│   ├── sub_agent_tools.py  # サブエージェント用
│   └── memory_tools.py     # メモリツール
├── memory/            # 記憶システム
│   ├── scratchpad.py       # スクラッチパッド
│   ├── kv_store.py         # Key-Valueストア
│   └── vector_db.py        # ベクトルデータベース
├── agent/             # エージェント実装
│   ├── base.py             # エージェント基底クラス
│   ├── sub_agent.py        # サブエージェント
│   └── context_manager.py  # コンテキスト管理
├── scoring/           # スコアリング
│   └── scorer.py           # 純資産計算
└── logging/           # ロギング
    ├── trace_logger.py     # ツール実行トレース
    └── metrics_logger.py   # 日次メトリクス
```

### 需要モデル（価格弾力性）

顧客購買行動は以下の式でモデル化されます：

```
expected_sales = base_sales × sales_impact × env_multiplier

sales_impact = 1 + elasticity × ((current_price - ref_price) / ref_price)

env_multiplier = weekday_mult × month_mult × weather_mult × choice_mult
```

- `elasticity`: 価格弾力性（通常-1〜-3、負の値）
- `ref_price`: 参照価格（「適正」と感じる価格）
- `base_sales`: 参照価格での基本売上数
- `choice_mult`: 商品バリエーションによる補正

### 純資産計算（スコアリング）

エージェントのパフォーマンスは純資産で評価されます：

```
純資産 = 口座残高 + 自販機内の現金 + 倉庫在庫価値 + 機内在庫価値
```

在庫価値は**仕入原価**で計算されます。

### メモリツール

長期記憶のために3種類のツールを提供：

1. **スクラッチパッド**: 自由形式のメモ（容量制限あり）
2. **Key-Valueストア**: 構造化データの永続保存
3. **ベクトルDB**: 類似検索対応のメモリ（TF-IDF/OpenAI埋め込み）

## 再現性

シミュレーションの再現性を保証するため、以下のシード値を設定できます：

```python
from vending_bench.config import Config

config = Config.default()
config.demand.seed = 42  # 需要モデルのシード
```

または設定ファイルで：

```yaml
demand:
  seed: 42
```

同じシードを使用した場合：
- 天候パターンが同一
- 顧客購買行動が同一
- 仕入先応答タイミングが同一

これにより、異なるエージェント実装間で公平な比較が可能です。

## テスト

```bash
# 全テスト実行
pytest

# 統合テスト
pytest tests/test_integration.py -v

# カバレッジ付き
pytest --cov=vending_bench
```

## ログ形式

### トレースログ (JSONL)

ツール実行の記録：

```json
{"timestamp": "2024-03-01T08:00:00", "day": 1, "tool": "get_money_balance", "success": true, "duration_ms": 5}
{"timestamp": "2024-03-01T08:01:00", "day": 1, "tool": "send_email", "success": true, "duration_ms": 120}
```

### メトリクスログ (JSONL)

日次サマリー：

```json
{
  "day": 1,
  "date": "2024-03-01",
  "balance": 498.00,
  "net_worth": 498.00,
  "units_sold": 0,
  "revenue": 0.00,
  "tool_calls": 5
}
```

## ライセンス

MIT License

## 参考文献

- 論文: "Vending-Bench: A Benchmark for Long-Form Agent Task Planning"
