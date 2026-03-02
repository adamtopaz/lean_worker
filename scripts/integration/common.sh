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
