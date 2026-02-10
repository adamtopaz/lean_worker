#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
BIN="$ROOT/.lake/build/bin/full_server"
PYTHON="${PYTHON:-python3}"

if ! command -v "$PYTHON" >/dev/null 2>&1; then
  PYTHON="python"
fi

if ! command -v "$PYTHON" >/dev/null 2>&1; then
  echo "python is required for integration scripts" >&2
  exit 1
fi

ensure_build() {
  (cd "$ROOT" && lake build full_server)
}

start_tcp_server() {
  local framing="$1"
  local log_level="${2:-warn}"
  local host="127.0.0.1"
  local port
  for port in $(seq 41000 41019); do
    "$BIN" --transport tcp --framing "$framing" --host "$host" --port "$port" --log-level "$log_level" &
    SERVER_PID=$!
    sleep 0.2
    if kill -0 "$SERVER_PID" >/dev/null 2>&1; then
      SERVER_PORT=$port
      return 0
    fi
    wait "$SERVER_PID" >/dev/null 2>&1 || true
  done
  echo "failed to start server" >&2
  return 1
}

stop_server() {
  if [ -n "${SERVER_PID:-}" ]; then
    kill "$SERVER_PID" >/dev/null 2>&1 || true
    wait "$SERVER_PID" >/dev/null 2>&1 || true
  fi
}
