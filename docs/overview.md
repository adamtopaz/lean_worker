# Architecture Overview

LeanWorker is an async-first JSON-RPC 2.0 framework for Lean 4.

## Layers

1. JSON-RPC types + validation (`LeanWorker/JsonRpc/*`)
2. Codec (`LeanWorker/Transport/Codec.lean`, `LeanWorker/JsonRpc/Codec.lean`)
3. Framing (byte payload boundaries in `LeanWorker/Framing/*`)
4. Protocol transports (`LeanWorker/Transport/Streams.lean`, `LeanWorker/Transport/Tcp.lean`, `LeanWorker/Transport/Spawn.lean`)
5. Async bridge (`LeanWorker/Async/Loops.lean`)
6. Server runtime (`LeanWorker/Server.lean`)
7. Client runtime (`LeanWorker/Client.lean`)
8. HTTP wrappers (`LeanWorker/Http/*`)
9. Tests + CLI (`LeanWorkerTest/*`, `scripts/integration/*`)

## Message Flow

```
protocol bytes (stdio/tcp)
  -> framing decode/encode (ByteArray frames)
  -> codec decode/encode (typed messages)
  -> Transport (Except JsonRpc.Error Incoming) Outgoing
  -> Server.run / Client.getClient
```

## Concurrency Model

- Server handlers run concurrently with optional `maxTasks` limits.
- Shared server state is synchronized with `Std.Mutex`.
- Client tracks pending requests with `Std.Mutex` + `IO.Promise`.

## Where to Start

- Core protocol + validation: `LeanWorker/JsonRpc/Core.lean`, `LeanWorker/JsonRpc/Parse.lean`
- Stream/TCP/spawn transport APIs: `LeanWorker/Transport/Streams.lean`, `LeanWorker/Transport/Tcp.lean`, `LeanWorker/Transport/Spawn.lean`
- Framing codecs: `LeanWorker/Framing/Newline.lean`, `LeanWorker/Framing/ContentLength.lean`, `LeanWorker/Framing/HttpLike.lean`
- Async bridging internals: `LeanWorker/Async/Loops.lean`
