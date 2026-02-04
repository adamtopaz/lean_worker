#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

"$ROOT/scripts/integration/stdio_newline.sh"
"$ROOT/scripts/integration/tcp_content_length.sh"
"$ROOT/scripts/integration/http_like.sh"

echo "integration suite ok"
