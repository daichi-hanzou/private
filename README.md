# Public-Information Business Planner MVP

企業の公開資料をローカルに配置し、売上成長目標を与えると、引用根拠付きの
現実的な成長施策を生成するCLIアプリです。MVPでは不正シミュレーション、
マルチエージェント、財務三表シミュレーションは扱いません。

## Setup

```powershell
uv sync
$env:OPENAI_API_KEY = "..."
```

## Data layout

```text
data/
└─ keyence/
   ├─ 01_company_profile/
   ├─ 02_financial_reports/
   ├─ 03_business_strategy/
   ├─ 04_business_risks/
   ├─ 05_peer_companies/
   │  ├─ omron/
   │  └─ fanuc/
   ├─ 06_industry_market/
   └─ 07_structured_data/
```

対応形式は PDF、TXT、Markdown、CSV、JSON、XLSX です。PDFの画像ページには
OCRを行わないため、テキスト抽出可能な資料を使用してください。

## Run

```powershell
uv run business-planner plan `
  --company-name Keyence `
  --target-revenue-growth 20%
```

結果は標準出力と `results/keyence/business_plan.json` に保存されます。
APIを呼ばず検索コンテキストだけ確認する場合:

```powershell
uv run business-planner inspect --company-name Keyence --query "売上 成長戦略"
```

別モデルを使う場合は `OPENAI_MODEL` を設定します。既定値は `gpt-5.6` です。

## Output

主要フィールドは以下です。

- `business_model_summary`
- `financial_summary`
- `key_growth_drivers`
- `growth_plan`
- `risk_assessment`
- `feasibility_assessment`
- `sources`

各施策には売上・利益への期待効果、必要投資、実装難易度、主要リスク、
根拠となる `source_id` が含まれます。金額や効果を公開資料から特定できない
場合、モデルには推測値を事実のように補完させず `null` と説明を返させます。

## Test

```powershell
uv run pytest
```
