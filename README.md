# LeanWorker

Lean 4 framework for building async-first JSON-RPC 2.0 servers and clients with pluggable transports and framings.

## Features
- Strict JSON-RPC 2.0 validation (version, ids, method rules, batches).
- Async server runtime with stateful/stateless handlers and notifications.
- Client runtime with pending request tracking and batch support.
- Transport and framing abstractions (newline, content-length, HTTP-like).
- Stdio subprocess client support (newline framing) plus end-to-end tests.

## Documentation

See `docs/README.md` for the full documentation suite.

## Repo Layout
- `LeanWorker/JsonRpc/Structured.lean`: structured params helpers.
- `LeanWorker/JsonRpc/Core.lean`: core JSON-RPC types.
- `LeanWorker/JsonRpc/Parse.lean`: parsing + validation helpers.
- `LeanWorker/JsonRpc/Encoding.lean`: encoding helpers.
- `LeanWorker/Transport/Types.lean`: transport abstraction.
- `LeanWorker/Transport/Streams.lean`: stdio stream byte transports.
- `LeanWorker/Transport/Logging.lean`: transport logging helpers.
- `LeanWorker/Transport/Spawn.lean`: spawn helpers for stdio transports.
- `LeanWorker/Framing/*`: newline, content-length, HTTP-like framings.
- `LeanWorker/Async/Loops.lean`: async loops bridging byte + JSON transports.
- `LeanWorker/Server.lean`: server API + runtime.
- `LeanWorker/Client.lean`: client API + runtime.
- `LeanWorkerTest/*`: test server, client helpers, and full test suite.
- `LeanWorkerTest/FullServer*.lean`: expanded test server + CLI entrypoint.
- `scripts/integration/*`: integration test harness scripts.

## Build
```bash
lake build
```

## Run
```bash
lake exe lean_worker
```

## Tests
```bash
lake build LeanWorkerTest
lake exe run_tests
```

Note: the parse-error test logs a framing decode error to stderr (expected).

## Integration Tests
```bash
scripts/integration/run.sh
```

Requirements: `python` or `python3` for stdio/TCP scripts, and `curl` for the HTTP-like script.
The HTTP-like invalid JSON case logs a parse-error line to stderr (expected).

## Full Test Server CLI
Build target: `full_server` (see `lakefile.toml`).

Example (stdio + newline):
```bash
lake exe full_server -- --transport stdio --framing newline
```

Example (tcp + http-like):
```bash
lake exe full_server -- --transport tcp --framing http-like --host 127.0.0.1 --port 41000 --log-level info
```

CLI flags:
- `--transport stdio|tcp`
- `--framing newline|content-length|http-like`
- `--host <host>` (tcp only)
- `--port <port>` (tcp only)
- `--log-level debug|info|warn|error`
- `--max-tasks <n>`
- `--name <string>`
- `--version <string>`
- `--delay-ms <n>`
- `--allow-custom-errors` / `--no-custom-errors`
- `--help`

## Expanded Test Server API
The expanded server is used by the CLI and integration scripts.

Requests:
- `ping` -> "pong"
- `info` -> `{ name, version, defaultDelayMs }`
- `echo` -> params as JSON
- `add` -> expects `{ a: Int, b: Int }`
- `sum` -> expects `[Int]`
- `sleep` -> expects `{ ms: Nat, tag?: String }`, returns `{ ms, tag? }`
- `counter.get` -> current counter
- `counter.add` -> expects `{ delta: Int }` (negative rejected)
- `counter.reset` -> returns previous counter
- `kv.get` -> expects `{ key: String }`, returns `{ found: Bool, value? }`
- `kv.set` -> expects `{ key: String, value: Json }`, returns `{ hadValue: Bool, oldValue? }`
- `kv.delete` -> expects `{ key: String }`, returns `Bool`
- `events.list` -> array of strings
- `events.clear` -> count cleared
- `state.snapshot` -> `{ counter, kvSize, eventsSize, inFlight }`
- `error.custom` -> expects `{ code: Int, message: String, data?: Json }`
- `error.internal` -> returns `Internal error`

Notifications:
- `notify.bump` -> `{ delta?: Nat }` increments counter
- `notify.log` -> `{ message: String }` appends to events
- `notify.sleep` -> `{ ms: Nat }` delays without response

## Server Example
```lean
open LeanWorker
open LeanWorker.JsonRpc
open LeanWorker.Server

def handlers : HandlerRegistry Unit Nat :=
  HandlerRegistry.empty
    |>.addStateless "ping" (fun _ => return "pong")

def notifications : NotificationRegistry Unit Nat :=
  NotificationRegistry.empty

def server : Server Unit Nat :=
  { handlers := handlers, notifications := notifications, transport := transport }

-- run with initial context/state
let state ← Std.Mutex.new 0
Async.block <| Server.run server () state
```

## Client Example
```lean
open LeanWorker

def main : IO Unit := do
  let client ← Async.block <| Client.spawnStdioClient ({ cmd := "./path/to/server" } : Transport.SpawnConfig)
  let result ← EIO.toIO' <| EAsync.block <| client.request "ping" none
  IO.println s!"{result}"
```

## Transport + Framing Examples

### Server over stdio (newline framing)
```lean
open LeanWorker
open LeanWorker.Server

def runServer : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let log ← Transport.stderrLogger "SERVER"
  let byteTransport ← Async.block <| Transport.byteTransportFromStreams stdin stdout log
  let transport ← Async.block <| Async.framedTransport byteTransport Framing.newline
  let server : Server Unit Nat :=
    { handlers := handlers, notifications := notifications, transport := transport }
  let state ← Std.Mutex.new 0
  Async.block <| Server.run server () state
```

### Server over stdio (content-length framing)
```lean
open LeanWorker

def runServer : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let log ← Transport.stderrLogger "SERVER"
  let byteTransport ← Async.block <| Transport.byteTransportFromStreams stdin stdout log
  let transport ← Async.block <| Async.framedTransport byteTransport Framing.contentLength
  let server : Server Unit Nat :=
    { handlers := handlers, notifications := notifications, transport := transport }
  let state ← Std.Mutex.new 0
  Async.block <| Server.run server () state
```

### Client over stdio (spawned subprocess, newline framing)
```lean
open LeanWorker

def main : IO Unit := do
  let client ← Async.block <| Client.spawnStdioClient ({ cmd := "./path/to/server" } : Transport.SpawnConfig)
  let result ← EIO.toIO' <| EAsync.block <| client.request "ping" none
  IO.println s!"{result}"
```

### Client with custom streams (content-length framing)
```lean
open LeanWorker

def connect (readStream writeStream : IO.FS.Stream) : IO Client.Client := do
  let log ← Transport.stderrLogger "CLIENT"
  let byteTransport ← Async.block <| Transport.byteTransportFromStreams readStream writeStream log
  let transport ← Async.block <| Async.framedTransport byteTransport Framing.contentLength
  Async.block <| Client.getClient transport
```

### HTTP-like framing
```lean
open LeanWorker

def connectHttpLike (readStream writeStream : IO.FS.Stream) : IO Client.Client := do
  let log ← Transport.stderrLogger "CLIENT"
  let byteTransport ← Async.block <| Transport.byteTransportFromStreams readStream writeStream log
  let transport ← Async.block <| Async.framedTransport byteTransport (Framing.httpLike {})
  Async.block <| Client.getClient transport
```

### HTTP server (TCP + HTTP-like framing)
```lean
open LeanWorker
open Std.Net

def httpAddr : SocketAddress :=
  SocketAddress.v4 { addr := IPv4Addr.ofParts 127 0 0 1, port := 8080 }

def runHttpServer : IO Unit := do
  let config : Http.ServerConfig := { addr := httpAddr }
  let listener ← Async.block <| Http.serve config fun transport =>
    let state ← Std.Mutex.new 0
    Server.run { handlers := handlers, notifications := notifications, transport := transport } () state
  -- listener.shutdown when you want to stop
  Async.block listener.shutdown
```

### HTTP client (TCP + HTTP-like framing)
```lean
open LeanWorker
open Std.Net

def httpAddr : SocketAddress :=
  SocketAddress.v4 { addr := IPv4Addr.ofParts 127 0 0 1, port := 8080 }

def main : IO Unit := do
  let config : Http.ClientConfig := { addr := httpAddr }
  let client ← Async.block <| Http.client config
  let result ← EIO.toIO' <| EAsync.block <| client.request "ping" none
  IO.println s!"{result}"
```

## Framing Options
- `Framing.newline` in `LeanWorker/Framing/Newline.lean`
- `Framing.contentLength` in `LeanWorker/Framing/ContentLength.lean`
- `Framing.httpLike` / `Framing.defaultHttpLike` in `LeanWorker/Framing/HttpLike.lean`

## Transport Options
- `Transport.byteTransportFromStreams` in `LeanWorker/Transport/Streams.lean` for `IO.FS.Stream`.
- `Transport.spawnStdioTransport` in `LeanWorker/Transport/Spawn.lean` for subprocess stdio (uses `Transport.SpawnConfig`).
- `Transport.Tcp.connectByteTransport` / `Transport.Tcp.listenByteTransport` in `LeanWorker/Transport/Tcp.lean`.
- Test-only line-based transport in `LeanWorkerTest/Support.lean`.
