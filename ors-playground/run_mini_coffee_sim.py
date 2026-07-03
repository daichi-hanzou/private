from ors.client import ORS

from mini_coffee_env import TOTAL_DAYS


BASE_URL = "http://localhost:8082"

# A small fixed policy so the environment runs end-to-end for all 7 days.
DAILY_PLAN = [
    {
        "buy": [("standard", 10), ("premium", 2)],
        "price": [("standard", 11.0), ("premium", 18.0)],
    },
    {
        "buy": [("standard", 8), ("premium", 2)],
        "price": [("standard", 11.0), ("premium", 18.0)],
    },
    {
        "buy": [("standard", 8), ("premium", 2)],
        "price": [("standard", 10.5), ("premium", 17.5)],
    },
    {
        "buy": [("standard", 14), ("premium", 4)],
        "price": [("standard", 9.5), ("premium", 16.5)],
    },
    {
        "buy": [("standard", 14), ("premium", 4)],
        "price": [("standard", 9.5), ("premium", 16.0)],
    },
    {
        "buy": [("standard", 8), ("premium", 2)],
        "price": [("standard", 10.5), ("premium", 17.0)],
    },
    {
        "buy": [("standard", 6), ("premium", 1)],
        "price": [("standard", 11.0), ("premium", 17.5)],
    },
]


def call_and_print(session, tool_name: str, args: dict) -> None:
    result = session.call_tool(tool_name, args)
    print(f"\n[{tool_name}]")
    for block in result.blocks:
        print(block.text)


def run_simulation() -> None:
    client = ORS(base_url=BASE_URL)
    env = client.environment("minicoffeeenv")
    task = env.list_tasks(split="train")[0]

    with env.session(task=task) as session:
        prompt = session.get_prompt()
        print("PROMPT")
        for block in prompt:
            print(block.text)

        for day_index in range(TOTAL_DAYS):
            day_number = day_index + 1
            print(f"\n{'=' * 20} Day {day_number} {'=' * 20}")

            call_and_print(session, "view_state", {})

            plan = DAILY_PLAN[day_index]
            for item_id, quantity in plan["buy"]:
                call_and_print(
                    session,
                    "buy_inventory",
                    {"item_id": item_id, "quantity_kg": quantity},
                )

            for item_id, price in plan["price"]:
                call_and_print(
                    session,
                    "set_price",
                    {"item_id": item_id, "price_per_kg": price},
                )

            call_and_print(session, "advance_day", {})

        print(f"\n{'=' * 18} Final Result {'=' * 18}")
        call_and_print(session, "finish_episode", {})


if __name__ == "__main__":
    run_simulation()
