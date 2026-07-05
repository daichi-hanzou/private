import os

from ors.client import ORS

from mini_coffee_env import TOTAL_DAYS


BASE_URL = os.getenv("MINI_COFFEE_ORS_URL", "http://localhost:8082")

DAILY_PLAN = [
    {
        "investigate": ["cloud_peak"],
        "spot": [("cloud_peak", "premium", 3)],
        "forward": [("sierra_verde", "standard", 8, 3)],
        "trader": [("standard", 6)],
        "price": [("standard", 11.0), ("premium", 18.2)],
    },
    {
        "investigate": ["riverbend"],
        "spot": [("riverbend", "standard", 5)],
        "forward": [("cloud_peak", "premium", 4, 4)],
        "trader": [("premium", 2)],
        "price": [("standard", 10.8), ("premium", 17.8)],
    },
    {
        "investigate": [],
        "spot": [],
        "forward": [("riverbend", "standard", 7, 5)],
        "trader": [("standard", 4)],
        "price": [("standard", 10.2), ("premium", 17.4)],
    },
    {
        "investigate": [],
        "spot": [("sierra_verde", "premium", 2)],
        "forward": [],
        "trader": [("standard", 6), ("premium", 1)],
        "price": [("standard", 9.8), ("premium", 16.8)],
    },
    {
        "investigate": [],
        "spot": [],
        "forward": [],
        "trader": [("standard", 5)],
        "price": [("standard", 9.8), ("premium", 16.5)],
    },
    {
        "investigate": [],
        "spot": [("riverbend", "standard", 4)],
        "forward": [],
        "trader": [("premium", 1)],
        "price": [("standard", 10.6), ("premium", 17.1)],
    },
    {
        "investigate": [],
        "spot": [],
        "forward": [],
        "trader": [("standard", 3)],
        "price": [("standard", 11.2), ("premium", 17.5)],
    },
]


def call_and_print(session, tool_name: str, args: dict) -> None:
    result = session.call_tool(tool_name, args)
    print(f"\n[{tool_name}]")
    for block in result.blocks:
        print(block.text)


def run_simulation() -> None:
    client = ORS(base_url=BASE_URL)
    try:
        env = client.environment("minicoffeeenv")
        task = env.list_tasks(split="train")[0]

        with env.session(task=task) as session:
            for block in session.get_prompt():
                print(block.text)

            for day_index in range(TOTAL_DAYS):
                print(f"\n{'=' * 20} Day {day_index + 1} {'=' * 20}")
                call_and_print(session, "view_state", {})
                plan = DAILY_PLAN[day_index]

                for farmer_id in plan["investigate"]:
                    call_and_print(session, "investigate_farmer", {"farmer_id": farmer_id})

                for farmer_id, item_id, quantity in plan["spot"]:
                    call_and_print(
                        session,
                        "buy_spot_direct",
                        {"farmer_id": farmer_id, "item_id": item_id, "quantity_kg": quantity},
                    )

                for farmer_id, item_id, quantity, delivery_day in plan["forward"]:
                    call_and_print(
                        session,
                        "create_forward_contract",
                        {
                            "farmer_id": farmer_id,
                            "item_id": item_id,
                            "quantity_kg": quantity,
                            "delivery_day": delivery_day,
                        },
                    )

                for item_id, quantity in plan["trader"]:
                    call_and_print(session, "buy_from_trader", {"item_id": item_id, "quantity_kg": quantity})

                for item_id, price in plan["price"]:
                    call_and_print(session, "set_price", {"item_id": item_id, "price_per_kg": price})

                call_and_print(session, "advance_day", {})

            print(f"\n{'=' * 18} Final Result {'=' * 18}")
            call_and_print(session, "finish_episode", {})
    finally:
        client.close()


if __name__ == "__main__":
    run_simulation()
