from ors.client import ORS


client = ORS(base_url="http://localhost:8082")
env = client.environment("minicoffeeenv")
task = env.list_tasks(split="train")[0]

with env.session(task=task) as session:
    prompt = session.get_prompt()
    print("PROMPT")
    print(prompt[0].text)

    print("\nSTATE")
    print(session.call_tool("view_state", {}).blocks[0].text)

    print("\nBUY")
    print(
        session.call_tool(
            "buy_inventory",
            {"item_id": "standard", "quantity_kg": 12},
        ).blocks[0].text
    )

    print("\nSET PRICE")
    print(
        session.call_tool(
            "set_price",
            {"item_id": "standard", "price_per_kg": 11.5},
        ).blocks[0].text
    )

    print("\nADVANCE DAY")
    print(session.call_tool("advance_day", {}).blocks[0].text)

    print("\nSTATE")
    print(session.call_tool("view_state", {}).blocks[0].text)
