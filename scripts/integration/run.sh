#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

"$ROOT/scripts/integration/stdio_newline.sh"
"$ROOT/scripts/integration/stdio_framed_streams.sh"

echo "integration suite ok"
