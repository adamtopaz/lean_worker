#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT/scripts/integration/common.sh"

ensure_build
start_tcp_server "content-length" "warn"
trap stop_server EXIT

REQ_JSON='{"jsonrpc":"2.0","id":1,"method":"sum","params":[1,2,3]}' \
EXPECT_RESULT='6' \
HOST="127.0.0.1" PORT="$SERVER_PORT" \
"$PYTHON" - <<'PY'
import json, os, socket

host=os.environ["HOST"]
port=int(os.environ["PORT"])
req=json.loads(os.environ["REQ_JSON"])
expected=int(os.environ["EXPECT_RESULT"])

body=json.dumps(req).encode()
msg=(f"Content-Length: {len(body)}\r\n\r\n").encode()+body

sock=socket.socket()
sock.connect((host, port))
sock.sendall(msg)

data=b""
while b"\r\n\r\n" not in data:
  chunk=sock.recv(4096)
  if not chunk:
    raise SystemExit("EOF before headers")
  data+=chunk

header, rest = data.split(b"\r\n\r\n", 1)
length=None
for line in header.split(b"\r\n"):
  if line.lower().startswith(b"content-length:"):
    length=int(line.split(b":",1)[1].strip() or b"0")
    break
if length is None:
  raise SystemExit("missing Content-Length")
while len(rest) < length:
  chunk=sock.recv(4096)
  if not chunk:
    raise SystemExit("EOF before body")
  rest+=chunk

body=rest[:length]
resp=json.loads(body)
assert resp.get("result") == expected, resp
assert resp.get("id") == 1, resp
sock.close()
PY

REQ_JSON='{"jsonrpc":"2.0","id":2,"method":"counter.add","params":{"delta":-1}}' \
HOST="127.0.0.1" PORT="$SERVER_PORT" \
"$PYTHON" - <<'PY'
import json, os, socket

host=os.environ["HOST"]
port=int(os.environ["PORT"])
req=json.loads(os.environ["REQ_JSON"])

body=json.dumps(req).encode()
msg=(f"Content-Length: {len(body)}\r\n\r\n").encode()+body

sock=socket.socket()
sock.connect((host, port))
sock.sendall(msg)

data=b""
while b"\r\n\r\n" not in data:
  chunk=sock.recv(4096)
  if not chunk:
    raise SystemExit("EOF before headers")
  data+=chunk

header, rest = data.split(b"\r\n\r\n", 1)
length=None
for line in header.split(b"\r\n"):
  if line.lower().startswith(b"content-length:"):
    length=int(line.split(b":",1)[1].strip() or b"0")
    break
if length is None:
  raise SystemExit("missing Content-Length")
while len(rest) < length:
  chunk=sock.recv(4096)
  if not chunk:
    raise SystemExit("EOF before body")
  rest+=chunk

body=rest[:length]
resp=json.loads(body)
err=resp.get("error") or {}
assert err.get("code") == -32602, resp
assert resp.get("id") == 2, resp
sock.close()
PY

echo "tcp content-length integration ok"
