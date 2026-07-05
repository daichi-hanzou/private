import os

from ors.client import ORS


client = ORS(base_url=os.getenv("MINI_COFFEE_ORS_URL", "http://localhost:8082"))
env = client.environment("minicoffeeenv")
task = env.list_tasks(split="train")[0]

with env.session(task=task) as session:
    print("PROMPT")
    print(session.get_prompt()[0].text)

    print("\nSTATE")
    print(session.call_tool("view_state", {}).blocks[0].text)

    print("\nINVESTIGATE")
    print(session.call_tool("investigate_farmer", {"farmer_id": "cloud_peak"}).blocks[0].text)

    print("\nSPOT DIRECT")
    print(
        session.call_tool(
            "buy_spot_direct",
            {"farmer_id": "cloud_peak", "item_id": "premium", "quantity_kg": 3},
        ).blocks[0].text
    )

    print("\nFORWARD CONTRACT")
    print(
        session.call_tool(
            "create_forward_contract",
            {"farmer_id": "sierra_verde", "item_id": "standard", "quantity_kg": 6, "delivery_day": 3},
        ).blocks[0].text
    )

    print("\nBUY TRADER")
    print(session.call_tool("buy_from_trader", {"item_id": "standard", "quantity_kg": 4}).blocks[0].text)

    print("\nADVANCE DAY")
    print(session.call_tool("advance_day", {}).blocks[0].text)
