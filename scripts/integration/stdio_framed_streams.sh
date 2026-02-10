#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT/scripts/integration/common.sh"

ensure_build

BIN="$BIN" "$PYTHON" - <<'PY'
import json
import os
import subprocess

bin_path = os.environ["BIN"]


class FramedClient:
    def __init__(self, framing: str):
        self.proc = subprocess.Popen(
            [bin_path, "--transport", "stdio", "--framing", framing, "--log-level", "warn"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
        )
        self._buffer = b""

    def close(self):
        self.proc.terminate()
        try:
            self.proc.wait(timeout=2)
        except subprocess.TimeoutExpired:
            self.proc.kill()

    def _read_exact(self, size: int) -> bytes:
        fd = self.proc.stdout.fileno()
        while len(self._buffer) < size:
            chunk = os.read(fd, 4096)
            if not chunk:
                raise SystemExit("EOF while reading framed response body")
            self._buffer += chunk
        result = self._buffer[:size]
        self._buffer = self._buffer[size:]
        return result

    def _read_headers(self) -> bytes:
        marker = b"\r\n\r\n"
        fd = self.proc.stdout.fileno()
        while marker not in self._buffer:
            chunk = os.read(fd, 4096)
            if not chunk:
                raise SystemExit("EOF while reading framed response headers")
            self._buffer += chunk
        header, rest = self._buffer.split(marker, 1)
        self._buffer = rest
        return header

    def recv_json(self):
        header = self._read_headers()
        length = None
        for line in header.split(b"\r\n"):
            if b":" not in line:
                continue
            key, value = line.split(b":", 1)
            if key.strip().lower() == b"content-length":
                length = int(value.strip() or b"0")
                break
        if length is None:
            raise SystemExit("missing Content-Length in response")
        body = self._read_exact(length)
        return json.loads(body)

    def send_raw(self, data: bytes):
        self.proc.stdin.write(data)
        self.proc.stdin.flush()

    def send_content_length(self, obj):
        body = json.dumps(obj).encode("utf-8")
        self.send_raw((f"Content-Length: {len(body)}\r\n\r\n").encode("utf-8") + body)

    def send_http_like(self, obj):
        body = json.dumps(obj).encode("utf-8")
        self.send_raw(
            b"POST / HTTP/1.1\r\n"
            + b"Content-Type: application/json\r\n"
            + (f"Content-Length: {len(body)}\r\n\r\n").encode("utf-8")
            + body
        )


def expect_parse_error(resp):
    err = resp.get("error") or {}
    assert err.get("code") == -32700, resp
    assert resp.get("id") is None, resp


def run_content_length_case():
    client = FramedClient("content-length")
    try:
        client.send_content_length({"jsonrpc": "2.0", "id": 1, "method": "ping"})
        resp = client.recv_json()
        assert resp.get("result") == "pong", resp
        assert resp.get("id") == 1, resp

        big = "x" * 20000
        client.send_content_length({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "echo",
            "params": {"big": big},
        })
        resp = client.recv_json()
        result = resp.get("result") or {}
        assert result.get("big") == big, resp
        assert resp.get("id") == 2, resp

        client.send_raw(b"X-Test: 1\r\n\r\n{}")
        resp = client.recv_json()
        expect_parse_error(resp)
    finally:
        client.close()


def run_http_like_case():
    client = FramedClient("http-like")
    try:
        client.send_http_like({"jsonrpc": "2.0", "id": 10, "method": "ping"})
        resp = client.recv_json()
        assert resp.get("result") == "pong", resp
        assert resp.get("id") == 10, resp

        big = "y" * 20000
        client.send_http_like({
            "jsonrpc": "2.0",
            "id": 11,
            "method": "echo",
            "params": {"big": big},
        })
        resp = client.recv_json()
        result = resp.get("result") or {}
        assert result.get("big") == big, resp
        assert resp.get("id") == 11, resp

        client.send_raw(b"\r\nContent-Length: 2\r\n\r\n{}")
        resp = client.recv_json()
        expect_parse_error(resp)
    finally:
        client.close()


run_content_length_case()
run_http_like_case()
print("stdio framed streams integration ok")
PY
