#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT/scripts/integration/common.sh"

ensure_build

BIN="$BIN" "$PYTHON" - <<'PY'
import json
import os
import select
import subprocess
import sys
import time

bin_path = os.environ["BIN"]

proc = subprocess.Popen(
  [bin_path, "--transport", "stdio", "--framing", "newline", "--log-level", "warn"],
  stdin=subprocess.PIPE,
  stdout=subprocess.PIPE,
  stderr=subprocess.DEVNULL,
  text=True,
)

def send(obj):
  proc.stdin.write(json.dumps(obj))
  proc.stdin.write("\n")
  proc.stdin.flush()
  line = proc.stdout.readline()
  if not line:
    raise SystemExit("EOF waiting for response")
  return json.loads(line)

resp = send({"jsonrpc": "2.0", "id": 1, "method": "ping"})
assert resp.get("result") == "pong", resp
assert resp.get("id") == 1, resp

resp = send({"jsonrpc": "2.0", "id": 2, "method": "add", "params": {"a": 1, "b": 2}})
assert resp.get("result") == 3, resp
assert resp.get("id") == 2, resp

resp = send({"jsonrpc": "2.0", "id": 3})
err = resp.get("error") or {}
assert err.get("code") == -32600, resp
assert resp.get("id") is None, resp

proc.stdin.write(json.dumps({"jsonrpc": "2.0", "method": "notify.log", "params": {"message": "hello"}}))
proc.stdin.write("\n")
proc.stdin.flush()

ready, _, _ = select.select([proc.stdout], [], [], 0.2)
if ready:
  line = proc.stdout.readline()
  raise SystemExit(f"unexpected notification response: {line}")

proc.terminate()
try:
  proc.wait(timeout=2)
except subprocess.TimeoutExpired:
  proc.kill()

print("stdio newline integration ok")
PY
