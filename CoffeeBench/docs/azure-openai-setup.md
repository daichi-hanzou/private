# CoffeeBench：Azure OpenAI接続

既存のOpenAI接続に加えて、Azure OpenAI v1 Responses APIを選べる。
CoffeeBench/.env の既存OpenAIキーはそのまま残し、以下を設定する。

```dotenv
COFFEEBENCH_OPENAI_PROVIDER=azure
AZURE_OPENAI_ENDPOINT=https://YOUR-RESOURCE.openai.azure.com
AZURE_OPENAI_DEPLOYMENT=YOUR-DEPLOYMENT-NAME
AZURE_OPENAI_MODEL=gpt-6-astra
AZURE_OPENAI_API_KEY=YOUR-AZURE-KEY
```

- ENDPOINTはリソースURLまたは `/openai/v1/` までのURL。デプロイやresponsesのパス、APIバージョンのクエリは付けない。
- DEPLOYMENTはAzureで作成したデプロイ名。APIリクエストにはこの名前を送る。
- MODELはデプロイの実モデル名。実験設定のモデル名（`:low`などを除く）と一致させる。対応済みモデルは `gpt-6-astra` と `gpt-5.5`。Azureでの提供・利用可否はアカウントやデプロイに依存し、本変更で提供を保証するものではない。
- `OPENAI_API_KEY` をAzureへの送信に流用しない。Azureで失敗した場合もOpenAIに自動切替しない。
- 環境変数がすでに設定されている場合は、`.env` より環境変数が優先される。

## 接続テスト

```bash
cd CoffeeBench
uv run python -m coffeebench.openai_smoke
```

1回の有料ツール呼び出しで認証・デプロイ・Responses/ツール対応を確認する。
`ok: true` と `provider: azure` を確認してから実験を開始する。

```bash
uv run python -m coffeebench.main --config experiments/circular/coordination_retention.toml --seed 0
```

通常OpenAIへ戻すには `COFFEEBENCH_OPENAI_PROVIDER=openai` とする。
モデル・実験条件は切替だけでは変更しない。同じモデルをAzureにデプロイする必要がある。
別のモデルを使う場合、実験設定と料金・推論設定の対応も必要になる。

## 記録と費用

provider・deployment・費用推計の根拠を出力に保存する。認証情報は保存しない。
Azureの価格は未確認のため、model_costはOpenAI Standard単価による参考推計であり、
Azureの実請求額ではない。SDK内部リトライは無効で、budget/react双方に対応する。

76件の自動テスト成功。Azure接続設定の検証、APIキー認証、HTTPリクエストのURL・認証・デプロイ名、
budget/react両方式の12日間模擬実行を確認した。Azureの実接続は未検証。

接続構造の参考：[OpenAI公式SDKのAzure v1 API説明](https://developers.openai.com/api/reference/ruby#microsoft-azure-openai)。
Python実装はインストール済みOpenAI SDKを使い、HTTPモックで検証している。
