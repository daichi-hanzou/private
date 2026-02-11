from __future__ import annotations

import json
import re
from datetime import datetime, timedelta
from typing import Any
from dotenv import load_dotenv

from openai import OpenAI
from openai.types.chat import ChatCompletionMessageParam

# Load environment variables from .env file
load_dotenv()

client = OpenAI()  # Will automatically use OPENAI_API_KEY from environment

# Date manager class
class DateManager:
    def __init__(self, start_date: datetime | None = None):
        self.current_date = start_date or datetime(2026, 1, 1)
        self.order_manager: OrderManager | None = None  # Will be set after OrderManager is created
        self.inventory: list | None = None  # Will be set to reference INVENTORY

    def get_current_date(self) -> str:
        return self.current_date.strftime("%Y-%m-%d")

    def advance_day(self, days: int = 1) -> str:
        self.current_date += timedelta(days=days)
        current_date_str = self.get_current_date()

        result: dict[str, Any] = {
            "status": "success",
            "current_date": current_date_str,
            "message": f"Advanced {days} day(s). Current date is now {current_date_str}"
        }

        # Check for deliveries if order_manager is available
        if self.order_manager and self.inventory is not None:
            delivered = self.order_manager.check_deliveries(current_date_str, self.inventory)
            if delivered:
                result["deliveries"] = delivered
                result["message"] += f"\n{len(delivered)} order(s) delivered!"

        return json.dumps(result, ensure_ascii=False, indent=2)

date_mgr = DateManager()

# Order manager class - tracks pending deliveries
class OrderManager:
    def __init__(self, date_manager):
        self.date_mgr = date_manager
        self.pending_orders = []
        self.order_counter = 0

    def place_order(self, product: str, quantity: int, supplier_email: str, expected_delivery_date: str | None = None) -> str:
        """Place an order with a supplier"""
        self.order_counter += 1
        order_id = f"ORDER-{self.order_counter:03d}"

        order = {
            "order_id": order_id,
            "product": product,
            "quantity": quantity,
            "supplier": supplier_email,
            "order_date": self.date_mgr.get_current_date(),
            "expected_delivery": expected_delivery_date,
            "status": "pending"
        }
        self.pending_orders.append(order)

        return json.dumps({
            "status": "success",
            "order_id": order_id,
            "message": f"Order placed for {quantity}x {product} from {supplier_email}",
            "expected_delivery": expected_delivery_date or "TBD"
        })

    def update_delivery_date(self, order_id: str, delivery_date: str) -> bool:
        """Update the expected delivery date for an order"""
        for order in self.pending_orders:
            if order["order_id"] == order_id:
                order["expected_delivery"] = delivery_date
                return True
        return False

    def check_deliveries(self, current_date: str, inventory: list) -> list:
        """Check if any orders should be delivered today and update inventory"""
        delivered = []
        remaining_orders = []

        for order in self.pending_orders:
            if order["expected_delivery"] == current_date:
                # Deliver the order - update inventory
                product_found = False
                for item in inventory:
                    if item["product"] == order["product"]:
                        item["quantity"] += order["quantity"]
                        product_found = True
                        break

                if not product_found:
                    # Product not in inventory, add it
                    inventory.append({
                        "product": order["product"],
                        "quantity": order["quantity"],
                        "price": "TBD"
                    })

                order["status"] = "delivered"
                delivered.append(order)
            else:
                remaining_orders.append(order)

        self.pending_orders = remaining_orders
        return delivered

    def get_pending_orders(self) -> str:
        """Get all pending orders"""
        return json.dumps(self.pending_orders, ensure_ascii=False, indent=2)

order_mgr = OrderManager(date_mgr)

# Link order manager to date manager and inventory
date_mgr.order_manager = order_mgr
# INVENTORY will be linked after it's defined below

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

# Link inventory to date manager
date_mgr.inventory = INVENTORY


# Supplier agent - generates replies with delivery date using OpenAI
class SupplierAgent:
    def __init__(self, openai_client, catalog, date_manager):
        self.client = openai_client
        self.catalog_json = json.dumps(catalog, ensure_ascii=False)
        self.date_mgr = date_manager

    def generate_reply(self, to: str, subject: str, body: str) -> dict:
        current_date = self.date_mgr.get_current_date()
        response = self.client.chat.completions.create(
            model="gpt-4o-mini",
            messages=[
                {
                    "role": "system",
                    "content": (
                        "You are a supplier sales representative. "
                        f"Today's date is {current_date}. "
                        "Reply to the incoming email with a delivery estimate of 2-4 days from today. "
                        "Specify the expected delivery date in your response. "
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
supplier_agent = SupplierAgent(client, CATALOG, date_mgr)
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
    {
        "type": "function",
        "function": {
            "name": "get_current_date",
            "description": "Get the current date in the simulation. Returns the current date in YYYY-MM-DD format.",
            "parameters": {
                "type": "object",
                "properties": {},
            },
        },
    },
    {
        "type": "function",
        "function": {
            "name": "advance_day",
            "description": "Advance the simulation to the next day. This moves time forward by the specified number of days (default 1 day). If there are pending orders with delivery dates, orders scheduled for the new date will be automatically delivered and added to inventory.",
            "parameters": {
                "type": "object",
                "properties": {
                    "days": {
                        "type": "integer",
                        "description": "Number of days to advance (default: 1)",
                        "default": 1,
                    },
                },
            },
        },
    },
    {
        "type": "function",
        "function": {
            "name": "place_order",
            "description": "Place an order with a supplier for a specific product and quantity. Use this after confirming delivery details from the supplier. You can optionally specify the expected delivery date if you already know it from the supplier's response.",
            "parameters": {
                "type": "object",
                "properties": {
                    "product": {"type": "string", "description": "Product name to order"},
                    "quantity": {"type": "integer", "description": "Quantity to order"},
                    "supplier_email": {"type": "string", "description": "Supplier's email address"},
                    "expected_delivery_date": {
                        "type": "string",
                        "description": "Expected delivery date in YYYY-MM-DD format (optional, can be updated later)",
                    },
                },
                "required": ["product", "quantity", "supplier_email"],
            },
        },
    },
    {
        "type": "function",
        "function": {
            "name": "get_pending_orders",
            "description": "Get a list of all pending orders that have been placed but not yet delivered. Shows order details including expected delivery dates.",
            "parameters": {
                "type": "object",
                "properties": {},
            },
        },
    },
    {
        "type": "function",
        "function": {
            "name": "extract_delivery_date",
            "description": "Extract the delivery date from a supplier's email response. Use this after reading a supplier's reply to automatically parse and extract the delivery date from the message body.",
            "parameters": {
                "type": "object",
                "properties": {
                    "email_body": {"type": "string", "description": "The body text of the supplier's email"},
                    "order_id": {
                        "type": "string",
                        "description": "The order ID to update with the extracted delivery date (optional)",
                    },
                },
                "required": ["email_body"],
            },
        },
    },
]


# Actual functions
def get_inventory() -> str:
    return json.dumps(INVENTORY, ensure_ascii=False)


def get_catalog() -> str:
    return json.dumps(CATALOG, ensure_ascii=False)


def extract_delivery_date(email_body: str, order_id: str | None = None) -> str:
    """Extract delivery date from supplier email using LLM"""
    try:
        response = client.chat.completions.create(
            model="gpt-4o-mini",
            messages=[
                {
                    "role": "system",
                    "content": (
                        "Extract the delivery date from the email text. "
                        f"Today's date is {date_mgr.get_current_date()}. "
                        "Return ONLY the date in YYYY-MM-DD format, nothing else. "
                        "If no specific date is mentioned, return 'NOT_FOUND'."
                    ),
                },
                {"role": "user", "content": email_body},
            ],
        )

        delivery_date = response.choices[0].message.content or "NOT_FOUND"
        delivery_date = delivery_date.strip()

        # Update order if order_id is provided
        if order_id and delivery_date != "NOT_FOUND":
            order_mgr.update_delivery_date(order_id, delivery_date)

        return json.dumps({
            "delivery_date": delivery_date,
            "order_updated": order_id if delivery_date != "NOT_FOUND" and order_id else None,
        })
    except Exception as e:
        return json.dumps({"error": str(e), "delivery_date": "NOT_FOUND"})


# Function dispatch
FUNCTIONS = {
    "get_inventory": get_inventory,
    "get_catalog": get_catalog,
    "send_email": lambda **kw: email_mgr.send_email(**kw),
    "get_inbox": lambda **kw: email_mgr.get_inbox(),
    "read_email": lambda **kw: email_mgr.read_email(**kw),
    "get_current_date": lambda **kw: json.dumps({"current_date": date_mgr.get_current_date()}),
    "advance_day": lambda **kw: date_mgr.advance_day(**kw),
    "place_order": lambda **kw: order_mgr.place_order(**kw),
    "get_pending_orders": lambda **kw: order_mgr.get_pending_orders(),
    "extract_delivery_date": lambda **kw: extract_delivery_date(**kw),
}

# Convert tools to a formatted string for the prompt
tools_description = json.dumps(tools, indent=2, ensure_ascii=False)

# 1. Set up procurement agent
messages: list[ChatCompletionMessageParam] = [
    {
        "role": "developer",
        "content": (
            "You are a procurement agent operating in a time-based simulation. Use the available tools to maintain adequate inventory levels.\n"
            "Check the warehouse inventory to see current stock levels. "
            "If a product is out of stock (quantity 0), check the product catalog to identify the supplier and their contact email. "
            "Send an inquiry email to the supplier asking about delivery time and availability. "
            "When a reply arrives, read it and extract the delivery date using the extract_delivery_date tool. "
            "Then place an order using the place_order tool with the extracted delivery date. "
            "Use the advance_day tool to move forward in time to the delivery date. "
            "When you advance to the delivery date, the order will be automatically delivered and added to inventory. "
            "Suppliers typically respond with delivery estimates of 2-4 days from the current date.\n"
            "Always use the email addresses found in the catalog. "
            "Your goal is to ensure we don't run out of stock by proactively ordering from suppliers.\n\n"
            "# Workflow:\n"
            "1. Check inventory and identify out-of-stock items\n"
            "2. Find supplier info from catalog\n"
            "3. Send inquiry email to supplier\n"
            "4. Read supplier's reply\n"
            "5. Extract delivery date from the reply using extract_delivery_date\n"
            "6. Place order with the extracted delivery date using place_order\n"
            "7. Advance time to the delivery date using advance_day\n"
            "8. Verify the inventory was updated (order automatically delivered)\n\n"
            "# Time Management:\n"
            "- Use get_current_date to check what day it is\n"
            "- Use advance_day to move time forward (deliveries happen automatically)\n"
            "- Use get_pending_orders to see all pending orders\n\n"
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

