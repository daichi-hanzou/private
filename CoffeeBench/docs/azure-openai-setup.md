# Azure OpenAI認証（2026-09-30更新）

AzureはDefaultAzureCredentialで認証します。APIキーは使用しません。

```dotenv
COFFEEBENCH_OPENAI_PROVIDER=azure
AZURE_OPENAI_ENDPOINT=https://YOUR-RESOURCE.openai.azure.com
AZURE_OPENAI_API_VERSION=YOUR-SUPPORTED-API-VERSION
AZURE_OPENAI_DEPLOYMENT=YOUR-DEPLOYMENT
AZURE_OPENAI_MODEL=gpt-5.6-sol
```

APIバージョンはリソースがResponses APIをサポートする値を指定してください。
エンドポイントはリソースのルートURLで、/openai/v1やクエリは付けません。
MODELは実験で指定する基盤モデル（:lowなどを除いたもの）と一致させます。

ローカルでは事前にAzure CLIの`az login`などで認証してください。
DefaultAzureCredentialが利用するIDには、対象Azure OpenAIを呼び出せる権限が必要です。

初期化時に`credential.get_token("https://cognitiveservices.azure.com/.default")`を呼び出し、
AzureOpenAIへトークンプロバイダーとして渡します。期限の5分前から再取得します。
トークン自体を.envや実験ログへ保存しません。認証失敗時はAPIキーやOpenAIへフォールバックしません。

`COFFEEBENCH_OPENAI_PROVIDER=openai`で通常のOpenAI接続を使用できます。
Claudeの接続や実験設定はこの切替の対象外です。

認証ヘッダー、APIバージョン、デプロイ名、トークン更新をモックで検証済みです。
実環境のAzure接続は未検証です。Azure費用はOpenAI価格による参考推計です。
