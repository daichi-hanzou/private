import unittest
from unittest.mock import patch

import mini_coffee_agent_runner as runner


class FakeLogger:
    def __init__(self):
        self.events = []

    def emit(self, event_type, **data):
        self.events.append((event_type, data))


class MiniCoffeeAgentRunnerTest(unittest.TestCase):
    def test_prompt_days_must_match_runner_total_days(self):
        runner._validate_prompt_total_days(f"You run a small coffee roaster-retailer for {runner.TOTAL_DAYS} days.")

        with self.assertRaisesRegex(RuntimeError, "Prompt horizon mismatch"):
            runner._validate_prompt_total_days(
                f"You run a small coffee roaster-retailer for {runner.TOTAL_DAYS + 1} days."
            )

    def test_max_turns_cleanup_advances_to_finish_episode(self):
        calls = []

        def fake_call(_session, tool_name, _arguments):
            calls.append(tool_name)
            if tool_name == "advance_day":
                return runner.AUTO_FINISH_MARKER, False, 0.0
            if tool_name == "finish_episode":
                return "Episode finished.\nProfit: $123.45", True, 0.6234
            raise AssertionError(tool_name)

        logger = FakeLogger()
        with patch.object(runner, "_call_ors_tool", side_effect=fake_call):
            reward = runner._force_finish_after_max_turns(object(), logger)

        self.assertEqual(reward, 0.6234)
        self.assertEqual(calls, ["advance_day", "finish_episode"])
        self.assertTrue(
            any(
                event_type == "tool_result"
                and data["tool"] == "finish_episode"
                and data["reason"] == "max_turns"
                and data["finished"]
                for event_type, data in logger.events
            )
        )


if __name__ == "__main__":
    unittest.main()
