# Azure OpenAI v1 Responses接続

以前の3社研究版（budget / ReAct両方）はAzure v1 Responses APIを使います。
Azure専用の認証・クライアント生成は `coffeebench/models/azure_client.py` にあります。
Responsesのツール・履歴変換は既存のOpenAIアダプターと共通です。

```dotenv
COFFEEBENCH_OPENAI_PROVIDER=azure
AZURE_OPENAI_ENDPOINT=https://YOUR-RESOURCE.openai.azure.com
AZURE_OPENAI_DEPLOYMENT=YOUR-DEPLOYMENT
# Optional: reference pricing only; leave blank for unknown cost
AZURE_OPENAI_MODEL=
```

`AZURE_OPENAI_API_VERSION` は不要です。既存.envに残っていても参照しません。
エンドポイントにはリソースのルートURLを指定します。コードで `/openai/v1/` を付け、
`client.responses.create` を呼びます。日付形式のapi-versionクエリは送りません。
モデルの指定には常にデプロイ名を使います。GPT-5.6 Solの実験にはそのモデルのデプロイを選んでください。

認証はDefaultAzureCredentialです。ローカルでは `az login` 等を事前に行い、
対象リソースへの推論権限を持つIDを使用します。
`get_token("https://cognitiveservices.azure.com/.default")` で得たトークンを、
`OpenAI(base_url=..., api_key=token_provider)` に渡します。
ここでapi_key引数はEntraトークンを返す関数で、APIキー認証ではありません。
期限の5分前から再取得します。認証失敗時にOpenAIやAPIキーへフォールバックしません。

`azure:low` は通常の行動判断・履歴要約とも `reasoning={"effort":"low"}` を送ります。
`azure:off` はreasoning指定の省略であり、推論無効化を保証しません。
エラー時にlowを自動で省略する処理はありません。
GPT-5.6のChat Completionsはツールとlowの併用に制限があるため、Responsesを使用します。

```bash
uv run python -m coffeebench.main --config experiments/circular/coordination_private_targets_gpt56_azure.toml --seed 0
```

モデルの料金名は任意の参考情報であり、接続先を決めません。
空欄・未知のモデルなら費用はnullです。指定した場合もOpenAI価格による参考値で、Azureの請求額ではありません。

認証・トークン更新・HTTPパス・ツールと推論履歴の受け渡し・12日間の3社実行をモックで検証しています。
実環境のAzure接続・循環の再現は未検証です。最小変更版ブランチのアダプターは今回の変更対象外です。

参考：
- https://learn.microsoft.com/en-us/azure/foundry/openai/api-version-lifecycle
- https://learn.microsoft.com/en-us/azure/foundry/openai/how-to/reasoning
- https://developers.openai.com/cookbook/examples/responses_api/reasoning_items

## 別環境への反映

ブランチを更新した後、CoffeeBenchフォルダで `uv sync --locked` を実行してください。
検証に使用したロック済みバージョンは openai 2.14.0、azure-identity 1.25.3、azure-core 1.41.0 です。
openaiを直接依存に追加し、azure-identityの最小版を検証済みの版へ引き上げています。
これは最新版への一括更新ではありません。

watchの消費者販売表示から廃止されたboost項目を除去し、取引日時から日を補完しました。
ログ・HTMLはUTF-8、CSVはUTF-8 BOM付きです。watchはWindows端末で表現できない文字をエスケープ表示します。
