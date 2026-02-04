#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT/scripts/integration/common.sh"

if ! command -v curl >/dev/null 2>&1; then
  echo "curl is required for http_like integration" >&2
  exit 1
fi

ensure_build
start_tcp_server "http-like" "warn"
trap stop_server EXIT

base_url="http://127.0.0.1:$SERVER_PORT/"

response=$(curl -sS --http1.1 -H 'Content-Type: application/json' -H 'Expect:' \
  --data '{"jsonrpc":"2.0","id":1,"method":"ping"}' "$base_url")
"$PYTHON" - "$response" <<'PY'
import json,sys
resp=json.loads(sys.argv[1])
assert resp.get("result") == "pong", resp
assert resp.get("id") == 1, resp
PY

response=$(curl -sS --http1.1 -H 'Content-Type: application/json' -H 'Expect:' \
  --data '{bad json' "$base_url")
"$PYTHON" - "$response" <<'PY'
import json,sys
resp=json.loads(sys.argv[1])
err=resp.get("error") or {}
assert err.get("code") == -32700, resp
assert resp.get("id") is None, resp
PY

response=$(curl -sS --http1.1 -H 'Content-Type: application/json' -H 'Expect:' \
  --data '{"jsonrpc":"2.0","id":2,"method":"kv.get","params":{"key":"missing"}}' "$base_url")
"$PYTHON" - "$response" <<'PY'
import json,sys
resp=json.loads(sys.argv[1])
result=resp.get("result") or {}
assert result.get("found") is False, resp
assert resp.get("id") == 2, resp
PY

echo "http-like integration ok"
