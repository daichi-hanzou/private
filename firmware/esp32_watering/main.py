import gc
import json
import network
import uasyncio as asyncio

import config
from pump import PumpController


pump = PumpController(config.PUMP_GPIO, config.PUMP_ACTIVE_HIGH, config.MAX_RUN_SECONDS)

if not config.API_TOKEN:
    raise RuntimeError("API_TOKEN must be configured")


def connect_wifi():
    station = network.WLAN(network.STA_IF)
    station.active(True)
    if not station.isconnected():
        station.connect(config.WIFI_SSID, config.WIFI_PASSWORD)
        while not station.isconnected():
            pass
    return station.ifconfig()[0]


async def send(writer, status, body):
    encoded = json.dumps(body).encode()
    reasons = {200: "OK", 202: "Accepted", 400: "Bad Request", 401: "Unauthorized", 404: "Not Found", 409: "Conflict"}
    writer.write(("HTTP/1.1 %d %s\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n" % (status, reasons.get(status, "Error"), len(encoded))).encode())
    writer.write(encoded)
    await writer.drain()
    await writer.wait_closed()


async def handle(reader, writer):
    try:
        line = await reader.readline()
        method, path, _version = line.decode().strip().split(" ")
        headers = {}
        while True:
            line = await reader.readline()
            if line == b"\r\n":
                break
            key, value = line.decode().split(":", 1)
            headers[key.casefold()] = value.strip()
        if headers.get("authorization") != "Bearer " + config.API_TOKEN:
            return await send(writer, 401, {"error": "unauthorized"})
        length = int(headers.get("content-length", "0"))
        if length > 4096:
            return await send(writer, 400, {"error": "request_too_large"})
        body = json.loads((await reader.readexactly(length)).decode()) if length else {}
        if method == "GET" and path == "/health":
            return await send(writer, 200, {"status": "ok"})
        if method == "GET" and path == "/status":
            return await send(writer, 200, pump.status())
        if method == "POST" and path == "/stop":
            return await send(writer, 200, {"stopped": pump.stop(), "pump_running": False})
        if method == "POST" and path == "/water":
            accepted, error = pump.start(body.get("duration_seconds"), body.get("request_id"))
            if accepted:
                return await send(writer, 202, {"accepted": True, "request_id": body["request_id"], "duration_seconds": body["duration_seconds"]})
            code = 409 if error in ("pump_already_running", "duplicate_request") else 400
            return await send(writer, code, {"accepted": False, "error": error})
        await send(writer, 404, {"error": "not_found"})
    except Exception:
        pump.stop()
        try:
            await send(writer, 400, {"error": "invalid_request"})
        except Exception:
            pass
    finally:
        gc.collect()


async def main():
    connect_wifi()
    asyncio.create_task(pump.watchdog())
    server = await asyncio.start_server(handle, "0.0.0.0", config.HTTP_PORT)
    while True:
        await asyncio.sleep(3600)


try:
    asyncio.run(main())
finally:
    pump.stop()
