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
OPENAI_MODEL=your-chat-deployment-name
OPENAI_EMBEDDING_MODEL=your-embedding-deployment-name
```

GPTとEmbeddingが別のAzureリソースにある場合は、個別のエンドポイントを設定します。

```dotenv
AZURE_OPENAI_CHAT_ENDPOINT=https://your-chat-resource.openai.azure.com/
AZURE_OPENAI_EMBEDDING_ENDPOINT=https://your-embedding-resource.openai.azure.com/
OPENAI_MODEL=your-chat-deployment-name
OPENAI_EMBEDDING_MODEL=your-embedding-deployment-name
```

個別設定がある場合はそれぞれを優先し、ない場合は`AZURE_OPENAI_ENDPOINT`を共通の
フォールバックとして使用します。`bm25`検索ではEmbeddingクライアントを作成しません。

いずれかのAzureエンドポイントが設定されている場合、CLIは`DefaultAzureCredential`で
`https://cognitiveservices.azure.com/.default`のトークンを取得します。取得したトークンは
`AZURE_OPENAI_AD_TOKEN`へ設定され、長時間実行中の更新にはBearerトークンプロバイダーが
使用されます。Azure OpenAIへのリクエストは
`<endpoint>/openai/v1/`のResponses APIへ送信されます。日付形式の
`OPENAI_API_VERSION`はこのv1接続では使用しません。ローカル開発では、Azure CLIのログインなど、
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
各ラウンドでEnvironment Agent、Execution Agent、Financial Engine、CEO Pressure、
Planner Revision、Internal Audit Observerを記録します。監査リスクスコアは
プログラムで決定論的に計算し、モデルは説明と推奨統制を生成します。
監査結果は事業実行リスク、財務報告リスク、不正圧力リスクを分けて表示します。
シミュレーションの計画期間と `business_plan.json` の計画期間が一致しない場合は
実行を停止するため、同じ年度指定で先に `plan` を再生成してください。

`--rounds` は1年を1ラウンドとして実行する回数です。指定回数より先に
`target-fiscal_year` に到達した場合は、その年度で自動停止します。各ラウンドでは
直前年度の改訂計画と財務結果を次年度へ引き継ぎます。APIコストを抑えて動作確認する場合は
`--rounds 1` を指定してください。

`plan`と`simulate`は、クエリごとの最大取得チャンク数を
`--retrieval-limit`で指定できます。指定範囲は1～50、デフォルトは
`plan`が5、`simulate`が4です。

```powershell
--retrieval-limit 12
```

取得件数を増やすとAPI入力トークンと関連性の低いチャンクも増えるため、
通常は6～12程度を推奨します。

### CEO・Plannerの最上位心得

会社別の `data/<会社名>/00_governance/executive_principles.json` が存在する場合、
初期Planner、CEO、Planner Revisionは、その内容を売上目標や短期KPIより優先する
最上位の経営判断基準として使用します。Reality AgentとInternal Audit Agentには
この設定ファイルを渡さず、検索根拠としても取り込みません。

いすゞ自動車用の設定は
`data/いすゞ自動車/00_governance/executive_principles.json` にあります。
別ファイルを使う場合は、`plan` と `simulate` の両方で次のように指定できます。

```powershell
--principles-file "path\to\executive_principles.json"
```

心得はデフォルトで有効です。比較実験などで無効にする場合は、`plan`または
`simulate`へ次を追加します。

```powershell
--principles-mode disabled
```

明示的に有効化する場合は次の指定です。

```powershell
--principles-mode enabled
```

初期計画から心得の影響を比較する場合は、`plan`と`simulate`で同じモードを指定して
ください。既存の計画を使い、CEOとPlanner Revisionだけを比較する場合は、
`simulate`のモードだけを切り替えられます。

研究用の高圧的企業文化テンプレートは
`templates/executive_principles/high_pressure_company_principles.json` にあります。
これは特定の実在企業を評価するものではなく、精神論・成果圧力を含む社是が
CEO・Plannerの判断へ与える影響を比較するためのテンプレートです。

```powershell
--principles-mode enabled `
--principles-file "templates\executive_principles\high_pressure_company_principles.json"
```

シミュレーションログの `settings.executive_principles` には、使用した方針の名称、
版、ファイル、SHA-256および可視範囲を保存します。心得本文はInternal Auditへの
入力には追加しません。

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
`simulation_analysis.strategy_evolution` には、年度別の施策追加・継続・拡大・縮小・
統合・置換・廃止、投資・人員・販促費・生産能力の配分、失敗パターンを記録します。

従来のReality Agentは3つの役割へ分割されています。

- Environment Agentは、需要、競争、為替、供給、コスト、規制、技術、品質などの
  外部環境だけを生成します。CEO、社是、評価制度は受け取りません。
- Execution Agentは、Plannerが明示した施策だけを外部環境下で通常実行し、
  施策別結果と翌年度候補を作ります。計画にない不適切行動を追加しません。
- Financial EngineはLLMを使わず、基準財務、外部圧力、施策別効果から売上、利益、
  キャッシュ、在庫および財務ブリッジを決定論的に集計します。

基準財務は公開資料中の丸め前の売上収益・営業利益・営業キャッシュフローから固定します。
各ラウンドのログには `environment_outcome`、`execution_outcome`、
`financial_engine`を別々に保存し、既存エージェント向けには互換性のある
`reality_outcome`統合ビューも保存します。

`failure_pattern`は、需要不足、供給障害、コスト上昇、規制遅延、技術遅延、
顧客採用遅延、品質問題など、外部環境または通常の実行失敗に限定します。
在庫押し込み、売上前倒し、過剰販促、値引き依存、販売金融条件の緩和などは、
Plannerが計画へ明示した場合に限りExecution Agentが実行結果として評価します。

また、各施策と代替候補について仮想パイプライン、成約率、1年間の売上機会、
営業利益率、推奨資源配分からなる `synthetic_internal_data` を生成します。
Planner Revisionは候補を選択し、施策を1～8件の範囲で追加・統合・置換・廃止できます。
売上・利益効果は選択候補から決定論的に算出されます。完全なログでは合成データとして
明示されますが、Planner向け実行報告では社内計画データとして提示されます。

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
