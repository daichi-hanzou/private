"""
株データを確認するツールを持つLLMエージェント
"""

import json
import os
from datetime import datetime, timedelta
from typing import Any

import yfinance as yf
from anthropic import Anthropic


# ツール定義
TOOLS = [
    {
        "name": "get_stock_price",
        "description": "指定した銘柄の現在の株価を取得します",
        "input_schema": {
            "type": "object",
            "properties": {
                "symbol": {
                    "type": "string",
                    "description": "株式のティッカーシンボル（例: AAPL, GOOGL, 7203.T）",
                }
            },
            "required": ["symbol"],
        },
    },
    {
        "name": "get_stock_history",
        "description": "指定した銘柄の過去の株価履歴を取得します",
        "input_schema": {
            "type": "object",
            "properties": {
                "symbol": {
                    "type": "string",
                    "description": "株式のティッカーシンボル（例: AAPL, GOOGL, 7203.T）",
                },
                "period": {
                    "type": "string",
                    "description": "取得期間（1d, 5d, 1mo, 3mo, 6mo, 1y, 2y, 5y, max）",
                    "default": "1mo",
                },
            },
            "required": ["symbol"],
        },
    },
    {
        "name": "get_stock_info",
        "description": "指定した銘柄の企業情報（時価総額、セクター、従業員数など）を取得します",
        "input_schema": {
            "type": "object",
            "properties": {
                "symbol": {
                    "type": "string",
                    "description": "株式のティッカーシンボル（例: AAPL, GOOGL, 7203.T）",
                }
            },
            "required": ["symbol"],
        },
    },
    {
        "name": "compare_stocks",
        "description": "複数の銘柄の株価パフォーマンスを比較します",
        "input_schema": {
            "type": "object",
            "properties": {
                "symbols": {
                    "type": "array",
                    "items": {"type": "string"},
                    "description": "比較する株式のティッカーシンボルのリスト",
                },
                "period": {
                    "type": "string",
                    "description": "比較期間（1mo, 3mo, 6mo, 1y）",
                    "default": "1mo",
                },
            },
            "required": ["symbols"],
        },
    },
]


def get_stock_price(symbol: str) -> dict[str, Any]:
    """現在の株価を取得"""
    try:
        stock = yf.Ticker(symbol)
        info = stock.info
        history = stock.history(period="1d")

        if history.empty:
            return {"error": f"銘柄 {symbol} のデータが見つかりません"}

        current_price = history["Close"].iloc[-1]
        open_price = history["Open"].iloc[-1]
        high = history["High"].iloc[-1]
        low = history["Low"].iloc[-1]
        volume = history["Volume"].iloc[-1]

        return {
            "symbol": symbol,
            "current_price": round(current_price, 2),
            "open": round(open_price, 2),
            "high": round(high, 2),
            "low": round(low, 2),
            "volume": int(volume),
            "currency": info.get("currency", "USD"),
            "name": info.get("shortName", symbol),
        }
    except Exception as e:
        return {"error": str(e)}


def get_stock_history(symbol: str, period: str = "1mo") -> dict[str, Any]:
    """株価履歴を取得"""
    try:
        stock = yf.Ticker(symbol)
        history = stock.history(period=period)

        if history.empty:
            return {"error": f"銘柄 {symbol} のデータが見つかりません"}

        # 直近10件のデータを返す
        recent = history.tail(10)
        data = []
        for date, row in recent.iterrows():
            data.append(
                {
                    "date": date.strftime("%Y-%m-%d"),
                    "open": round(row["Open"], 2),
                    "high": round(row["High"], 2),
                    "low": round(row["Low"], 2),
                    "close": round(row["Close"], 2),
                    "volume": int(row["Volume"]),
                }
            )

        # パフォーマンス計算
        start_price = history["Close"].iloc[0]
        end_price = history["Close"].iloc[-1]
        change_percent = ((end_price - start_price) / start_price) * 100

        return {
            "symbol": symbol,
            "period": period,
            "data_points": len(history),
            "recent_data": data,
            "period_change_percent": round(change_percent, 2),
            "period_high": round(history["High"].max(), 2),
            "period_low": round(history["Low"].min(), 2),
        }
    except Exception as e:
        return {"error": str(e)}


def get_stock_info(symbol: str) -> dict[str, Any]:
    """企業情報を取得"""
    try:
        stock = yf.Ticker(symbol)
        info = stock.info

        return {
            "symbol": symbol,
            "name": info.get("shortName", "N/A"),
            "sector": info.get("sector", "N/A"),
            "industry": info.get("industry", "N/A"),
            "market_cap": info.get("marketCap", "N/A"),
            "pe_ratio": info.get("trailingPE", "N/A"),
            "dividend_yield": info.get("dividendYield", "N/A"),
            "52_week_high": info.get("fiftyTwoWeekHigh", "N/A"),
            "52_week_low": info.get("fiftyTwoWeekLow", "N/A"),
            "employees": info.get("fullTimeEmployees", "N/A"),
            "website": info.get("website", "N/A"),
            "description": info.get("longBusinessSummary", "N/A")[:500]
            if info.get("longBusinessSummary")
            else "N/A",
        }
    except Exception as e:
        return {"error": str(e)}


def compare_stocks(symbols: list[str], period: str = "1mo") -> dict[str, Any]:
    """複数銘柄を比較"""
    try:
        results = []
        for symbol in symbols:
            stock = yf.Ticker(symbol)
            history = stock.history(period=period)

            if not history.empty:
                start_price = history["Close"].iloc[0]
                end_price = history["Close"].iloc[-1]
                change_percent = ((end_price - start_price) / start_price) * 100

                results.append(
                    {
                        "symbol": symbol,
                        "name": stock.info.get("shortName", symbol),
                        "start_price": round(start_price, 2),
                        "end_price": round(end_price, 2),
                        "change_percent": round(change_percent, 2),
                        "volatility": round(history["Close"].std(), 2),
                    }
                )

        # パフォーマンス順にソート
        results.sort(key=lambda x: x["change_percent"], reverse=True)

        return {"period": period, "comparison": results}
    except Exception as e:
        return {"error": str(e)}


def execute_tool(tool_name: str, tool_input: dict) -> str:
    """ツールを実行"""
    if tool_name == "get_stock_price":
        result = get_stock_price(tool_input["symbol"])
    elif tool_name == "get_stock_history":
        result = get_stock_history(
            tool_input["symbol"], tool_input.get("period", "1mo")
        )
    elif tool_name == "get_stock_info":
        result = get_stock_info(tool_input["symbol"])
    elif tool_name == "compare_stocks":
        result = compare_stocks(
            tool_input["symbols"], tool_input.get("period", "1mo")
        )
    else:
        result = {"error": f"未知のツール: {tool_name}"}

    return json.dumps(result, ensure_ascii=False, indent=2)


def run_agent(user_message: str):
    """エージェントを実行"""
    client = Anthropic()

    print(f"\n{'='*60}")
    print(f"ユーザー: {user_message}")
    print(f"{'='*60}")

    messages = [{"role": "user", "content": user_message}]

    system_prompt = """あなたは株式市場の専門家アシスタントです。
ユーザーの質問に答えるために、利用可能なツールを使って株価データを取得し、分析してください。
日本株の場合は銘柄コードに.Tを付けてください（例: トヨタ→7203.T）。
データを取得したら、わかりやすく解説してください。"""

    # エージェントループ
    while True:
        response = client.messages.create(
            model="claude-sonnet-4-20250514",
            max_tokens=4096,
            system=system_prompt,
            tools=TOOLS,
            messages=messages,
        )

        # レスポンスを処理
        assistant_content = response.content
        messages.append({"role": "assistant", "content": assistant_content})

        # ツール使用があるかチェック
        tool_uses = [block for block in assistant_content if block.type == "tool_use"]

        if not tool_uses:
            # ツール使用がなければ、テキストを出力して終了
            for block in assistant_content:
                if hasattr(block, "text"):
                    print(f"\nアシスタント: {block.text}")
            break

        # ツールを実行
        tool_results = []
        for tool_use in tool_uses:
            print(f"\n[ツール実行: {tool_use.name}]")
            print(f"  入力: {json.dumps(tool_use.input, ensure_ascii=False)}")

            result = execute_tool(tool_use.name, tool_use.input)
            print(f"  結果: {result[:200]}..." if len(result) > 200 else f"  結果: {result}")

            tool_results.append(
                {
                    "type": "tool_result",
                    "tool_use_id": tool_use.id,
                    "content": result,
                }
            )

        messages.append({"role": "user", "content": tool_results})


def main():
    """メイン関数"""
    print("株式データエージェント")
    print("終了するには 'quit' または 'exit' と入力してください")
    print("-" * 60)

    while True:
        try:
            user_input = input("\n質問を入力: ").strip()

            if not user_input:
                continue

            if user_input.lower() in ["quit", "exit", "q"]:
                print("終了します。")
                break

            run_agent(user_input)

        except KeyboardInterrupt:
            print("\n終了します。")
            break


if __name__ == "__main__":
    main()
