from __future__ import annotations

import argparse
import json
import time

from .client import ESP32Client, WateringClientError
from .config import WateringConfig
from .history import WateringHistory
from .service import WateringService
from .simulator import serve


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="watering")
    commands = parser.add_subparsers(dest="command", required=True)
    for name in ("water", "simulate"):
        command = commands.add_parser(name)
        command.add_argument("--seconds", type=int)
    commands.add_parser("health")
    commands.add_parser("status")
    commands.add_parser("stop")
    scheduler = commands.add_parser("run-scheduler")
    scheduler.add_argument("--poll-seconds", type=float, default=30)
    simulator = commands.add_parser("run-simulator")
    simulator.add_argument("--host", default="127.0.0.1")
    simulator.add_argument("--port", type=int, default=8788)
    return parser


def main() -> None:
    parser = _parser()
    args = parser.parse_args()
    try:
        config = WateringConfig.from_env()
        if args.command == "run-simulator":
            serve(args.host, args.port, config.esp32_api_token, config.max_duration_seconds)
            return
        client = ESP32Client(
            config.esp32_base_url, config.esp32_api_token,
            timeout=config.request_timeout_seconds,
        )
        if args.command in {"health", "status", "stop"}:
            print(json.dumps(getattr(client, args.command)(), ensure_ascii=False, indent=2))
            return
        with WateringHistory(config.state_db, config.log_path) as history:
            service = WateringService(config, client, history)
            if args.command in {"water", "simulate"}:
                seconds = args.seconds or config.watering_duration_seconds
                response = service.water(
                    seconds,
                    method="simulation" if args.command == "simulate" else "manual",
                )
                print(json.dumps(response, ensure_ascii=False, indent=2))
                return
            if args.poll_seconds <= 0:
                raise ValueError("poll interval must be positive")
            print(f"Watering scheduler: {config.watering_time} ({config.timezone})")
            while True:
                service.run_scheduled_once()
                time.sleep(args.poll_seconds)
    except (OSError, ValueError, WateringClientError) as exc:
        parser.error(str(exc))


if __name__ == "__main__":
    main()
