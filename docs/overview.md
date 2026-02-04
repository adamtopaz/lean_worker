# Architecture Overview

LeanWorker is an async-first JSON-RPC 2.0 framework for Lean 4. It splits concerns into distinct layers:

1. **JSON-RPC types and validation**: request/response parsing, strict spec validation.
2. **Structured params**: typed decoding of `params` and typed encoding of results.
3. **Transport**: abstract message channels with logging and shutdown semantics.
4. **Framing**: encode/decode JSON to raw bytes (newline, content-length, HTTP-like).
5. **Async loops**: glue between byte transports and JSON transports.
6. **Server runtime**: handler registries, notification dispatch, concurrency control.
7. **Client runtime**: request/notify/batch APIs, pending response tracking.
8. **TCP/HTTP utilities**: socket-based transports and HTTP-like helpers.
9. **Test server and CLI**: expanded example API, CLI flags, integration scripts.

## Message Flow

```
ByteTransport (TCP/stdio)
  -> Framing.encode/decode
  -> Transport (Json inbox/outbox)
  -> Server.run / Client.getClient
```

Incoming bytes are decoded into JSON values. The server parses JSON-RPC messages, runs handlers, and pushes responses back through the transport. Notifications never produce responses.

## Concurrency Model

- Server handlers run concurrently with a configurable `maxTasks` limit.
- State mutations are serialized through a `Std.Mutex State`.
- The client tracks in-flight requests via `Std.Mutex` + `IO.Promise`.

## Where to Look

- Core JSON-RPC types: `LeanWorker/JsonRpc/Types.lean`
- Structured params: `LeanWorker/JsonRpc/Structured.lean`
- Transport and framing: `LeanWorker/Transport/Types.lean`, `LeanWorker/Framing/*`
- Async loops: `LeanWorker/Async/Loops.lean`
- Server/client: `LeanWorker/Server.lean`, `LeanWorker/Client.lean`
- TCP/HTTP utilities: `LeanWorker/Transport/Tcp.lean`, `LeanWorker/Http/*`
