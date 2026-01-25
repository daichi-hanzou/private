# Phase 1: Vending-Bench 論文準拠仕様書

## 概要

Phase 1は、論文「Vending-Bench: A Benchmark for Long-Term Coherence of Autonomous Agents」に準拠した、
ベースラインとなる自動販売機ビジネスシミュレーション環境を実装します。

## 1. オペレーターエージェント（Claudius相当）

### Role: Operator Agent

主要エージェント。自動販売機ビジネスの日次オペレーションを全て担当します。

### 責務

1. **仕入れ管理**
   - サプライヤーへのメール送信による発注
   - 配送状況の追跡
   - 在庫コストの管理

2. **価格設定**
   - 商品ごとの価格決定
   - 需要・競合に基づく価格調整
   - 利益率の最適化

3. **補充オペレーション**
   - 倉庫から自販機へのプロダクト補充
   - スロット配置の最適化
   - 在庫切れ防止

4. **在庫管理**
   - 倉庫在庫の追跡
   - 自販機内在庫の監視
   - 発注タイミングの判断

5. **メール対応**
   - サプライヤーとのコミュニケーション
   - 発注・配送確認
   - 請求書処理

6. **日次オペレーション**
   - 現金回収
   - 日次費用の支払い
   - 売上レポートの確認

### 構造

#### Loop-based Architecture

```
while not terminated:
    1. 環境から観測（observation）を受け取る
    2. think() で次のアクションを決定
    3. ツールを実行
    4. 結果を受け取り、履歴に追加
    5. コンテキストウィンドウ管理
```

#### Context Window Truncation

- **制限**: デフォルト 30,000 トークン
- **管理戦略**:
  - 古いメッセージから削除
  - システムプロンプトは常に保持
  - 直近の重要な情報を優先
  - メモリツールへの退避を推奨

#### Memory Tools（3種類）

**1. Scratchpad（スクラッチパッド）**
- 自由形式のテキストメモ
- 容量制限: 5,000文字
- 用途: 短期的な計画、メモ、TODO管理

**2. Key-Value Store**
- 構造化データの永続保存
- 容量: 無制限（実用上100エントリ程度）
- 用途: 重要な数値、状態、設定の保存
- 例:
  - `last_order_date:product_name` → "2024-03-15"
  - `target_stock:Coca-Cola` → "30"

**3. Vector Memory（TF-IDF + cosine similarity）**
- セマンティック検索対応のメモリ
- 埋め込み: TF-IDF（MVP）/ OpenAI Embeddings（オプション）
- 類似度: cosine similarity
- 用途: 過去の学習・経験の検索
- 例:
  - "Red Bullが売れなかった理由"
  - "週末の売上パターン"

---

## 2. サブエージェント（Field Sub-Agent）

### Role: 物理作業の実行者

オペレーターエージェントから指示を受け、自販機での物理作業を実行します。

### 利用可能ツール

#### `stock_products_from_storage_to_machine`
- 倉庫から自販機へ商品を補充
- パラメータ:
  - `product_name: str`
  - `quantity: int`
  - `target_row: int`
  - `target_column: int`
- コスト: 75分

#### `collect_cash_from_machine`
- 自販機内の現金を回収し、口座へ入金
- パラメータ: なし
- コスト: 25分

#### `set_prices`
- 自販機のスロット価格を設定
- パラメータ:
  - `prices: list[dict]`  # [{row, column, price}, ...]
- コスト: 25分

#### `get_machine_inventory`
- 自販機の在庫状況を取得
- パラメータ: なし
- コスト: 5分

### サブエージェントとの対話モデル

```
Operator: run_sub_agent(task="Stock Coca-Cola in machine")
↓
Sub-Agent: プランニング → ツール実行
↓
Operator: 結果を受け取る
```

---

## 3. 需要・販売シミュレーション

### 価格弾力性モデル（Price Elasticity of Demand）

顧客の購買行動を以下の式でモデル化します。

#### 基本式

```
expected_sales = base_sales × sales_impact × env_multiplier

sales_impact = 1 + elasticity × ((current_price - ref_price) / ref_price)

env_multiplier = weekday_mult × month_mult × weather_mult × choice_mult
```

#### パラメータ説明

**商品固有パラメータ（ProductDemandParams）**

| パラメータ | 意味 | 典型的な値 |
|-----------|------|-----------|
| `price_elasticity` | 価格弾力性（負の値） | -1.0 〜 -3.0 |
| `reference_price` | 参照価格（「適正」と感じる価格） | 商品カテゴリ依存 |
| `base_sales` | 参照価格での1日あたり基本売上数 | 2 〜 12 units/day |

**環境係数（Multipliers）**

1. **曜日係数（weekday_multipliers）**
   - 月〜日: `[0.8, 0.85, 0.9, 0.95, 1.1, 1.3, 1.2]`
   - 週末に売上増

2. **月別係数（month_multipliers）**
   - 1〜12月: `[0.7, 0.75, 0.85, 0.95, 1.0, 1.15, 1.2, 1.15, 1.0, 0.95, 0.85, 0.9]`
   - 夏に売上増

3. **天候係数（weather_multipliers）**
   - sunny: 1.1
   - cloudy: 1.0
   - rainy: 0.85

4. **品揃え効果（assortment effect / choice_multiplier）**
   - 最適商品種類数: 6
   - 種類が少ない: ペナルティ（0.7〜1.0）
   - 種類が多すぎる: ペナルティ（最大50%減）

#### ノイズと制約

- **ノイズ**: ガウス分布（標準偏差 = `noise_std × expected_sales`、デフォルト 0.1）
- **在庫上限**: 計算結果を `min(expected_sales, slot.quantity)` で制限
- **整数化**: 最終的な販売数は整数に丸める

---

## 4. 環境パラメータ（論文デフォルト）

### 初期設定

```yaml
environment:
  initial_balance: 500.0          # 初期資金（ドル）
  daily_fee: 2.0                  # 日次運営費（ドル）
  start_date: "2025-01-01"        # シミュレーション開始日

agent:
  max_messages: 2000              # 最大メッセージ数
  context_tokens: 30000           # コンテキストトークン上限
  bankruptcy_threshold: 10        # 連続未払い日数で破綻

machine:
  rows: 4                         # 自販機の行数
  slots_per_row: 3                # 各行のスロット数
  # 合計12スロット
```

### 破綻条件

- **10日連続で日次費用未払い** → ゲームオーバー
- 条件:
  - `account.balance < daily_fee`
  - 連続日数が `bankruptcy_threshold` に達する

---

## 5. スコアリング（評価指標）

### Net Worth（純資産）

エージェントのパフォーマンスは **純資産** で評価されます。

#### 計算式

```python
net_worth = (
    account.balance                     # 口座残高
    + machine.cash_inside               # 自販機内の現金
    + warehouse_inventory_value         # 倉庫在庫価値
    + machine_inventory_value           # 機内在庫価値
)
```

#### 在庫価値の評価

- **評価基準**: 仕入原価（wholesale cost）
- 例:
  - Coca-Cola 10個 @ $0.50/個 = $5.00
  - Red Bull 5個 @ $1.20/個 = $6.00

#### スコアリング頻度

- 日次で計算
- ログに記録: `metrics.jsonl`

---

## 6. ツール一覧とコスト（時間単位：分）

### メインエージェント用ツール

| ツール名 | 機能 | 時間コスト |
|---------|------|-----------|
| `read_emails` | メール一覧取得 | 5分 |
| `read_email` | 特定メール詳細 | 5分 |
| `send_email` | メール送信 | 25分 |
| `get_money_balance` | 口座残高確認 | 5分 |
| `get_storage_inventory` | 倉庫在庫確認 | 5分 |
| `wait_for_next_day` | 翌日へ進む | 0分 |
| `write_scratchpad` | スクラッチパッド書込 | 5分 |
| `read_scratchpad` | スクラッチパッド読込 | 5分 |
| `set_kv_value` | KVストア書込 | 5分 |
| `get_kv_value` | KVストア読込 | 5分 |
| `add_to_vector_db` | ベクトルDB追加 | 5分 |
| `search_vector_db` | ベクトルDB検索 | 5分 |
| `run_sub_agent` | サブエージェント実行 | 300分 |

### サブエージェント用ツール

| ツール名 | 機能 | 時間コスト |
|---------|------|-----------|
| `stock_products_from_storage_to_machine` | 商品補充 | 75分 |
| `collect_cash_from_machine` | 現金回収 | 25分 |
| `set_prices` | 価格設定 | 25分 |
| `get_machine_inventory` | 機内在庫確認 | 5分 |

---

## 7. シミュレーション詳細

### サプライヤーシステム

#### メールベースの発注

1. **発注メール送信**
   - 宛先: `supplier@example.com`
   - 件名: "Order Request"
   - 本文:
     ```
     I would like to order:
     - Coca-Cola: 30 units
     - Red Bull: 20 units

     Account: [account_number]
     Delivery Address: [delivery_address]
     ```

2. **サプライヤー応答**
   - 配送予定日: 2〜5日後
   - 請求額の通知
   - 口座からの自動引き落とし

3. **商品配送**
   - 指定日に倉庫へ配送
   - メールで配送完了通知

### 天候システム

- 毎日ランダムに決定（シード固定で再現可能）
- 月別で確率分布変更
  - 夏: 晴れ70%、曇り20%、雨10%
  - 冬: 晴れ30%、曇り40%、雨30%

### 時間進行

- **1日 = 480分** （8時間営業想定）
- ツール実行で時間経過
- `wait_for_next_day` で日付変更
  - 売上シミュレーション実行
  - 日次費用引き落とし

---

## 8. ログとトレーシング

### トレースログ（trace.jsonl）

ツール実行の記録:

```json
{
  "timestamp": "2025-01-01T08:00:00",
  "day": 1,
  "tool": "get_money_balance",
  "arguments": {},
  "result": "500.00",
  "success": true,
  "duration_ms": 5,
  "time_cost_minutes": 5
}
```

### メトリクスログ（metrics.jsonl）

日次サマリー:

```json
{
  "day": 1,
  "date": "2025-01-01",
  "balance": 498.00,
  "machine_cash": 0.00,
  "net_worth": 498.00,
  "units_sold": 0,
  "revenue": 0.00,
  "tool_calls": 8,
  "weather": "sunny"
}
```

---

## 9. 実装チェックリスト

### 必須コンポーネント

- [x] オペレーターエージェント基底クラス
- [x] サブエージェント実装
- [x] コンテキスト管理（30,000トークン制限）
- [x] メモリツール3種（Scratchpad, KV, Vector）
- [x] 需要モデル（価格弾力性）
- [x] サプライヤーシステム（メールベース）
- [x] 環境状態管理（口座、在庫、時計）
- [x] スコアリング（純資産計算）
- [x] ログシステム（trace, metrics）

### 評価基準

1. **再現性**: 同じシードで同じ結果
2. **公平性**: 全エージェントが同じ初期条件
3. **挑戦性**: 2000メッセージで長期的戦略が必要
4. **現実性**: 価格弾力性が実際の購買行動を反映

---

## 10. 実行方法

### 基本実行

```bash
# Phase 1 ベースライン実行
python -m vending_bench.run --mode phase1_baseline --seed 42

# 設定ファイル指定
python -m vending_bench.run --config configs/phase1.yaml

# 最大日数制限
python -m vending_bench.run --max-days 30
```

### 評価

```bash
# 純資産の推移をプロット
python -m vending_bench.evaluate --metrics output/metrics.jsonl

# トレースログ分析
python -m vending_bench.analyze_trace --trace output/trace.jsonl
```

---

## 11. Phase 1 の設計原則

### シンプルさ

- 論文仕様に忠実
- 余計な機能を追加しない
- ベースライン性能の測定が目的

### 再現性

- すべての乱数にシード設定
- 決定論的な動作
- 比較実験可能

### 拡張性

- Phase 2 への拡張を考慮した設計
- モジュール化された構造
- 追加コンポーネントの統合が容易

---

## 参考文献

- 論文: "Vending-Bench: A Benchmark for Long-Term Coherence of Autonomous Agents"
- セクション 2.2: Environment Design
- セクション 2.2.2: Demand Simulation
- セクション 2.3: Agent Architecture
