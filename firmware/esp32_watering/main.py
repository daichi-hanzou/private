import gc
import json
import network
import time
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
        timeout_seconds = getattr(config, "WIFI_CONNECT_TIMEOUT_SECONDS", 30)
        deadline = time.ticks_add(time.ticks_ms(), timeout_seconds * 1000)
        while not station.isconnected():
            if time.ticks_diff(deadline, time.ticks_ms()) <= 0:
                station.disconnect()
                raise RuntimeError("wifi_connection_timeout")
            time.sleep_ms(100)
    ip_address = station.ifconfig()[0]
    print("Wi-Fi connected; IP address:", ip_address)
    return ip_address


def safe_error(exc):
    message = str(exc)
    for secret in (config.WIFI_SSID, config.WIFI_PASSWORD, config.API_TOKEN):
        if secret:
            message = message.replace(secret, "[redacted]")
    return type(exc).__name__ + ": " + message


async def send(writer, status, body):
    encoded = json.dumps(body).encode()
    reasons = {200: "OK", 202: "Accepted", 400: "Bad Request", 401: "Unauthorized", 404: "Not Found", 409: "Conflict"}
    writer.write(("HTTP/1.1 %d %s\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n" % (status, reasons.get(status, "Error"), len(encoded))).encode())
    writer.write(encoded)
    await writer.drain()
    writer.close()
    try:
        await writer.wait_closed()
    except AttributeError:
        # Older MicroPython StreamWriter variants close without wait_closed().
        pass


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
            headers[key.strip().lower()] = value.strip()
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
    except Exception as exc:
        print("HTTP request error:", safe_error(exc))
        pump.stop()
        try:
            await send(writer, 400, {"error": "invalid_request"})
        except Exception as send_exc:
            print("HTTP error response failed:", safe_error(send_exc))
    finally:
        gc.collect()


async def main():
    ip_address = connect_wifi()
    asyncio.create_task(pump.watchdog())
    server = await asyncio.start_server(handle, "0.0.0.0", config.HTTP_PORT)
    print("Watering API listening on http://%s:%d" % (ip_address, config.HTTP_PORT))
    while True:
        await asyncio.sleep(3600)


try:
    asyncio.run(main())
except Exception as exc:
    print("Fatal watering service error:", safe_error(exc))
    raise
finally:
    pump.stop()
