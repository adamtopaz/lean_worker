# LeanWorker

Lean 4 framework for async JSON-RPC 2.0 servers and clients.

Current scope is intentionally narrow:

- JSON-RPC parsing/validation/encoding
- server + client runtimes
- stdio transport
- framing modes: newline and content-length

TCP and HTTP-like transport support were removed and will be revisited later when Lean core HTTP support is available.

## Build

```bash
lake build
```

## Tests

```bash
lake exe run_tests
```

## Full Server CLI

Build:

```bash
lake build full_server
```

Run (stdio + newline):

```bash
lake exe full_server -- --transport stdio --framing newline
```

Run (stdio + content-length):

```bash
lake exe full_server -- --transport stdio --framing content-length
```

## Main Modules

- `LeanWorker/JsonRpc/*`: JSON-RPC types, parsing, encoding, structured params
- `LeanWorker/Transport/Types.lean`: `ServerTransport` and `ClientTransport`
- `LeanWorker/Transport/Streams.lean`: stream-based JSON transports
- `LeanWorker/Transport/Spawn.lean`: spawn stdio client transport helper
- `LeanWorker/Transport/Logging.lean`: async logging/channel helpers
- `LeanWorker/Framing/*`: newline/content-length framing
- `LeanWorker/Server/*`: server registry/types/runtime
- `LeanWorker/Client/*`: client runtime and request/notify/batch APIs
- `LeanWorkerTest/*`: tests, full test server, and CLI

For detailed docs, see `docs/README.md`.
