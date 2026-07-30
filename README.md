# Public-Information Business Planner MVP

企業の公開資料をローカルに配置し、売上成長目標を与えると、引用根拠付きの
現実的な成長施策を生成するCLIアプリです。MVPでは不正シミュレーション、
マルチエージェント、財務三表シミュレーションは扱いません。

## Setup

```powershell
uv sync
```

`.env.example` を参考に `.env` の `OPENAI_API_KEY` を設定してください。
CLIの起動時にプロジェクト直下の `.env` が自動的に読み込まれます。

Azure OpenAIをMicrosoft Entra IDで利用する場合は、`OPENAI_API_KEY`の代わりに
次の値を設定します。

```dotenv
AZURE_OPENAI_ENDPOINT=https://your-resource.openai.azure.com/
OPENAI_API_VERSION=your_azure_openai_api_version
OPENAI_MODEL=your-chat-deployment-name
OPENAI_EMBEDDING_MODEL=your-embedding-deployment-name
```

`AZURE_OPENAI_ENDPOINT`が設定されている場合、CLIは`DefaultAzureCredential`で
`https://cognitiveservices.azure.com/.default`のトークンを取得します。取得したトークンは
`AZURE_OPENAI_AD_TOKEN`へ設定され、長時間実行中の更新にはBearerトークンプロバイダーが
使用されます。ローカル開発では、Azure CLIのログインなど、
`DefaultAzureCredential`が利用できる認証を事前に完了してください。

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
  --target-revenue-growth 20% `
  --base-fiscal-year 2026 `
  --target-fiscal-year 2031
```

結果は標準出力と `results/keyence/business_plan.json` に保存されます。
既定では `text-embedding-3-large` による意味検索とBM25を組み合わせた
ハイブリッド検索を使用します。埋め込みモデルは `OPENAI_EMBEDDING_MODEL` で変更できます。
APIを使わず従来のキーワード検索だけを使う場合は `--retrieval bm25` を指定します。
APIを呼ばず検索コンテキストだけ確認する場合:

```powershell
uv run business-planner inspect --company-name Keyence --query "売上 成長戦略" --retrieval bm25
```

別モデルを使う場合は `OPENAI_MODEL` を設定します。既定値は `gpt-5.6` です。

## One-round simulation

既存の `business_plan.json` を1年間実行して目標未達になった合成シナリオを作成し、
CEOの叱責と売上KPIへの集中、Plannerによる計画改訂、内部監査の受動的観察を
1ラウンド実行します。合成財務数値は実績ではなく、公開資料にあるリスクを基にした
研究用シナリオとして明示されます。

```powershell
uv run business-planner simulate `
  --company-name "いすゞ自動車" `
  --target-revenue-growth 20% `
  --base-fiscal-year 2026 `
  --target-fiscal-year 2031 `
  --retrieval hybrid `
  --ceo-pressure high `
  --rounds 5
```

結果は `results/<会社名>/simulation_runs/run_<UTC時刻>.json` に保存されます。
初期MVPでは複数ラウンドは行いません。Reality Agent、CEO Pressure、
Planner Revision、Internal Audit Observerの4段階を記録します。監査リスクスコアは
プログラムで決定論的に計算し、モデルは説明と推奨統制を生成します。
監査結果は事業実行リスク、財務報告リスク、不正圧力リスクを分けて表示します。
シミュレーションの計画期間と `business_plan.json` の計画期間が一致しない場合は
実行を停止するため、同じ年度指定で先に `plan` を再生成してください。

`--rounds` は1年を1ラウンドとして実行する回数です。指定回数より先に
`target-fiscal_year` に到達した場合は、その年度で自動停止します。各ラウンドでは
直前年度の改訂計画と財務結果を次年度へ引き継ぎます。APIコストを抑えて動作確認する場合は
`--rounds 1` を指定してください。

`--target-revenue-growth 20%` は毎年度20%ではなく、`base_fiscal_year` から
`target_fiscal_year` までの累計売上成長目標です。シミュレーターは基準年度売上から
最終年度の目標売上を固定し、各ラウンドの開始時点で、残存年度と直前年度売上から
当年度に必要な成長率をCAGRとして再計算します。ログには累計目標、最終年度目標売上、
当年度必要成長率、累計実現成長率、目標までの残存成長率を別々に保存します。

Planner Agentには `planner_execution_report` だけを渡します。このビューには実行財務、
施策結果、失敗理由、社内計画データが含まれますが、仮想シナリオであることを示す免責文、
生成前提、`synthetic_*` メタデータは含まれません。完全な `reality_outcome` と免責情報は、
利用者・監査向けの実行ログには保存されます。

`simulation_analysis.timeline` には年度別の財務、CEO圧力、主要KPI、監査リスクを保存し、
`simulation_analysis.optimization_drift` には売上KPI偏重の推移を0–100の指標と
`Increasing`、`Stable`、`Decreasing` のトレンドで記録します。

Reality Agent は公開資料中の丸め前の売上収益・営業利益・営業キャッシュフローを
基準財務として固定します。合成した翌年度数値は `financial_bridge` により、
基礎的な増減、各施策の効果、その他要因へ分解され、合計が翌年度数値と一致する場合だけ
採用されます。

また、各施策について仮想パイプライン、成約率、1年間の売上機会、営業利益率からなる
`synthetic_internal_data` を生成します。Planner Revision の売上・利益効果はこの値から
算出され、`仮想シナリオ推計` と明示されます。これらは公開実績、会社予想、
経営上のコミットメントではありません。

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
