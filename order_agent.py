import json
import re

from openai import OpenAI
from openai.types.chat import ChatCompletionMessageParam

client = OpenAI()

# Product catalog data
CATALOG = {
    "Toyota": {
        "company_name": "Toyota Motor Corporation",
        "address": "1 Toyota-cho, Toyota City, Aichi, Japan",
        "email": "info@toyota.co.jp",
        "products": [
            {"name": "Corolla", "category": "Sedan", "price": "2,016,000 JPY~"},
            {"name": "Prius", "category": "Hybrid", "price": "2,750,000 JPY~"},
            {"name": "Yaris", "category": "Compact", "price": "1,501,000 JPY~"},
            {"name": "RAV4", "category": "SUV", "price": "2,938,000 JPY~"},
            {"name": "Crown", "category": "Sedan", "price": "4,350,000 JPY~"},
        ],
    },
    "Honda": {
        "company_name": "Honda Motor Co., Ltd.",
        "address": "2-1-1 Minami-Aoyama, Minato-ku, Tokyo, Japan",
        "email": "info@honda.co.jp",
        "products": [
            {"name": "Fit", "category": "Compact", "price": "1,624,800 JPY~"},
            {"name": "Civic", "category": "Sedan", "price": "3,190,000 JPY~"},
            {"name": "Vezel", "category": "SUV", "price": "2,279,200 JPY~"},
            {"name": "N-BOX", "category": "Kei Car", "price": "1,648,900 JPY~"},
            {"name": "Step WGN", "category": "Minivan", "price": "3,053,600 JPY~"},
        ],
    },
    "Nissan": {
        "company_name": "Nissan Motor Co., Ltd.",
        "address": "1-1-1 Takashima, Nishi-ku, Yokohama, Kanagawa, Japan",
        "email": "info@nissan.co.jp",
        "products": [
            {"name": "Note", "category": "Compact", "price": "2,299,000 JPY~"},
            {"name": "Serena", "category": "Minivan", "price": "2,768,700 JPY~"},
            {"name": "Leaf", "category": "EV", "price": "3,531,000 JPY~"},
            {"name": "X-Trail", "category": "SUV", "price": "3,510,100 JPY~"},
            {"name": "Skyline", "category": "Sedan", "price": "4,514,400 JPY~"},
        ],
    },
}

# Warehouse inventory data
INVENTORY = [
    {"product": "Corolla", "quantity": 5, "price": "2,016,000 JPY"},
    {"product": "Prius", "quantity": 0, "price": "2,750,000 JPY"},
    {"product": "Yaris", "quantity": 3, "price": "1,501,000 JPY"},
    {"product": "Fit", "quantity": 2, "price": "1,624,800 JPY"},
    {"product": "Civic", "quantity": 0, "price": "3,190,000 JPY"},
    {"product": "N-BOX", "quantity": 8, "price": "1,648,900 JPY"},
    {"product": "Note", "quantity": 4, "price": "2,299,000 JPY"},
    {"product": "Leaf", "quantity": 0, "price": "3,531,000 JPY"},
    {"product": "X-Trail", "quantity": 1, "price": "3,510,100 JPY"},
]


# Supplier agent - generates replies with delivery date using OpenAI
class SupplierAgent:
    def __init__(self, openai_client, catalog):
        self.client = openai_client
        self.catalog_json = json.dumps(catalog, ensure_ascii=False)

    def generate_reply(self, to: str, subject: str, body: str) -> dict:
        response = self.client.chat.completions.create(
            model="gpt-4o-mini",
            messages=[
                {
                    "role": "system",
                    "content": (
                        "You are a supplier sales representative. "
                        "Reply to the incoming email with a delivery estimate (e.g. 2-4 weeks). "
                        "Be professional and concise. Reply in English.\n\n"
                        f"Product catalog:\n{self.catalog_json}"
                    ),
                },
                {
                    "role": "user",
                    "content": f"From: {to}\nSubject: {subject}\n\n{body}",
                },
            ],
        )
        return {
            "from": to,
            "subject": f"Re: {subject}",
            "body": response.choices[0].message.content,
        }


# Email manager class
class EmailManager:
    def __init__(self):
        self.sent = {}
        self.inbox = {
            "RECV-001": {"from": "info@toyota.co.jp", "subject": "New Prius Model Announcement", "body": "The 2026 Prius model has been released.", "status": "unread"},
            "RECV-002": {"from": "info@honda.co.jp", "subject": "N-BOX Special Price Campaign", "body": "N-BOX is available at a special price for a limited time.", "status": "unread"},
            "RECV-003": {"from": "info@nissan.co.jp", "subject": "Leaf Test Drive Event", "body": "A Leaf test drive event will be held next Saturday.", "status": "unread"},
        }
        self._send_counter = 0
        self._recv_counter = 3
        self._on_send_listeners = []

    def on_send(self, callback):
        self._on_send_listeners.append(callback)

    def _add_to_inbox(self, email: dict):
        self._recv_counter += 1
        email_id = f"RECV-{self._recv_counter:03d}"
        self.inbox[email_id] = {**email, "status": "unread"}
        return email_id

    def read_email(self, email_id: str) -> str:
        if email_id in self.inbox:
            self.inbox[email_id]["status"] = "read"
            return json.dumps({"email_id": email_id, **self.inbox[email_id]})
        return json.dumps({"error": f"{email_id} not found"})

    def send_email(self, to: str, subject: str, body: str) -> str:
        self._send_counter += 1
        email_id = f"SEND-{self._send_counter:03d}"
        self.sent[email_id] = {"to": to, "subject": subject, "body": body, "status": "sent"}
        result = {"status": "sent", "email_id": email_id}
        # Fire event listeners
        for listener in self._on_send_listeners:
            reply = listener(to=to, subject=subject, body=body)
            if reply:
                reply_id = self._add_to_inbox(reply)
                result["reply_received"] = reply_id
                print(f"\n  [Event] Supplier replied -> {reply_id}")
                print(f"  From: {reply['from']}")
                print(f"  Subject: {reply['subject']}")
                print(f"  Body: {reply['body']}\n")
        return json.dumps(result)

    def get_inbox(self) -> str:
        summary = {eid: {"subject": m["subject"], "status": m["status"]} for eid, m in self.inbox.items()}
        return json.dumps(summary)


email_mgr = EmailManager()
supplier_agent = SupplierAgent(client, CATALOG)
email_mgr.on_send(supplier_agent.generate_reply)



# Tool definitions (プロンプト内に文字列として埋め込む)
tools = [
    {
        "type": "function",
        "function": {
            "name": "get_inventory",
            "description": "Get the current warehouse inventory. Returns each product's name, quantity in stock, and price.",
            "parameters": {
                "type": "object",
                "properties": {},
            },
        },
    },
    {
        "type": "function",
        "function": {
            "name": "get_catalog",
            "description": "Get the full product catalog for all client companies. Returns company name, address, email, and product list.",
            "parameters": {
                "type": "object",
                "properties": {},
            },
        },
    },
    {
        "type": "function",
        "function": {
            "name": "send_email",
            "description": "Create and send an email. Does not actually send; assigns an ID and saves it.",
            "parameters": {
                "type": "object",
                "properties": {
                    "to": {"type": "string", "description": "Recipient email address"},
                    "subject": {"type": "string", "description": "Email subject"},
                    "body": {"type": "string", "description": "Email body"},
                },
                "required": ["to", "subject", "body"],
            },
        },
    },
    {
        "type": "function",
        "function": {
            "name": "get_inbox",
            "description": "Get the inbox summary. Returns each email's ID, subject, and status (unread/read).",
            "parameters": {
                "type": "object",
                "properties": {},
            },
        },
    },
    {
        "type": "function",
        "function": {
            "name": "read_email",
            "description": "Read a specific received email by ID. Returns full details (from, subject, body) and marks it as read.",
            "parameters": {
                "type": "object",
                "properties": {
                    "email_id": {"type": "string", "description": "Email ID (e.g. RECV-001)"},
                },
                "required": ["email_id"],
            },
        },
    },
]


# Actual functions
def get_inventory() -> str:
    return json.dumps(INVENTORY, ensure_ascii=False)


def get_catalog() -> str:
    return json.dumps(CATALOG, ensure_ascii=False)


# Function dispatch
FUNCTIONS = {
    "get_inventory": get_inventory,
    "get_catalog": get_catalog,
    "send_email": lambda **kw: email_mgr.send_email(**kw),
    "get_inbox": lambda **kw: email_mgr.get_inbox(),
    "read_email": lambda **kw: email_mgr.read_email(**kw),
}

# Convert tools to a formatted string for the prompt
tools_description = json.dumps(tools, indent=2, ensure_ascii=False)

# 1. Set up procurement agent
messages: list[ChatCompletionMessageParam] = [
    {
        "role": "developer",
        "content": (
            "You are a procurement agent. Use the available tools to maintain adequate inventory levels.\n"
            "Check the warehouse inventory to see current stock levels. "
            "If a product is out of stock (quantity 0), check the product catalog to identify the supplier and their contact email. "
            "Send an inquiry email to the supplier asking about delivery time and availability. "
            "When a reply arrives, read it and summarize the result.\n"
            "Always use the email addresses found in the catalog. "
            "Your goal is to ensure we don't run out of stock by proactively ordering from suppliers.\n\n"
            "# Available Tools:\n"
            f"{tools_description}\n\n"
            "# Response Format:\n"
            "You MUST always follow this format:\n"
            "1. First, output your thinking process under <thinking> tags\n"
            "2. Then, if you need to call a tool, output the tool call in JSON format under <tool_call> tags\n\n"
            "Example:\n"
            "<thinking>\n"
            "I need to check the inventory first to see if Prius is in stock.\n"
            "</thinking>\n\n"
            "<tool_call>\n"
            '{"name": "get_inventory", "arguments": {}}\n'
            "</tool_call>\n\n"
            "If you don't need to call a tool, just output your thinking and final answer.\n"
            "Always include <thinking> tags to show your reasoning process."
        ),
    },
    {"role": "user", "content": "Check if Prius is in stock. If not, contact the supplier and ask about delivery time."},
]

def extract_thinking(content: str) -> str:
    """Extract thinking process from <thinking> tags"""
    match = re.search(r'<thinking>(.*?)</thinking>', content, re.DOTALL)
    return match.group(1).strip() if match else ""

def extract_tool_call(content: str) -> dict | None:
    """Extract tool call from <tool_call> tags"""
    match = re.search(r'<tool_call>(.*?)</tool_call>', content, re.DOTALL)
    if match:
        try:
            return json.loads(match.group(1).strip())
        except json.JSONDecodeError:
            return None
    return None

# 2. Loop while the model requests function calls
MAX_STEPS = 10
MAX_RETRIES = 3
step = 0
retry_count = 0

while step < MAX_STEPS:
    step += 1

    # API呼び出し（ループ内で実行）
    response = client.chat.completions.create(model="gpt-4o-mini", messages=messages)
    content = response.choices[0].message.content or ""

    # Extract and print thinking process
    thinking = extract_thinking(content)
    if thinking:
        print(f"\n[Step {step} - Thinking]\n{thinking}\n")

    # Extract tool call
    tool_call = extract_tool_call(content)

    if not tool_call:
        retry_count += 1
        print(f"[Warning] No tool call found. Retry {retry_count}/{MAX_RETRIES}")

        if retry_count >= MAX_RETRIES:
            # 3回リトライしても見つからなければ最終回答として扱う
            final_answer = re.sub(r'<(thinking|tool_call)>.*?</\1>', '', content, flags=re.DOTALL)
            print(f"[Final Answer]\n{final_answer.strip()}")
            break

        # リトライを促すメッセージを追加
        messages.append({"role": "assistant", "content": content})
        messages.append({"role": "user", "content": "Please call a tool if needed, or provide your final answer with <thinking> tags."})
        continue

    # ツール呼び出しが成功したらリトライカウントをリセット
    retry_count = 0

    # Add assistant's message to history
    messages.append({"role": "assistant", "content": content})

    # Execute tool
    tool_name = tool_call.get("name")
    tool_args = tool_call.get("arguments", {})

    print(f"[Step {step} - Tool: {tool_name}]\nArguments: {tool_args}")

    if tool_name not in FUNCTIONS:
        print(f"Error: Unknown tool '{tool_name}'")
        break

    result = FUNCTIONS[tool_name](**tool_args)
    print(f"Result: {result[:200]}{'...' if len(result) > 200 else ''}\n")

    messages.append({"role": "user", "content": f"Tool execution result:\n{result}"})

