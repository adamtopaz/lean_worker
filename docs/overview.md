# Architecture Overview

LeanWorker is an async-first JSON-RPC 2.0 runtime for Lean 4.

## Layers

1. JSON-RPC types + validation (`LeanWorker/JsonRpc/*`)
2. Framing (`LeanWorker/Framing/*`)
3. Role-specific transports (`LeanWorker/Transport/Types.lean`)
4. Stream/spawn transport constructors (`LeanWorker/Transport/Streams.lean`, `LeanWorker/Transport/Spawn.lean`)
5. Server runtime (`LeanWorker/Server/*`)
6. Client runtime (`LeanWorker/Client/*`)
7. Tests + CLI (`LeanWorkerTest/*`, `scripts/integration/*`)

## Message Flow

```text
stdio bytes
  -> framing decode (newline/content-length)
  -> UTF-8 + JSON decode
  -> ServerTransport/ClientTransport inbox
  -> Server.run / Client.getClient
```

Outgoing flow is the reverse: JSON -> UTF-8 bytes -> framing encode -> stdio stream writes.

## Concurrency Model

- Server handles requests concurrently (optional `maxTasks`).
- Mutable server state is synchronized with `Std.Mutex`.
- Client tracks pending requests with `Std.Mutex` + `IO.Promise`.

## Scope

- Supported today: stdio transport + newline/content-length framing.
- Deferred: TCP/HTTP transport until core Lean HTTP support is available.
