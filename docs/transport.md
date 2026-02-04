# Transport Abstraction

Transports are defined in `LeanWorker/Transport/Types.lean`. A transport exposes inbox/outbox channels, logging, and shutdown semantics.

## Type

```lean
structure Transport (Incoming Outgoing : Type) where
  inbox : Std.CloseableChannel Incoming
  outbox : Std.CloseableChannel Outgoing
  log : LogLevel → String → IO Unit
  shutdown : Async Unit
```

`ByteTransport` is an alias for `Transport ByteArray ByteArray`.

## Key Ideas

- `inbox` is the stream of incoming messages.
- `outbox` is used to send outgoing messages.
- `log` is for operational logging; never write JSON-RPC payloads to stderr.
- `shutdown` should close loops and underlying resources.

## Usage Patterns

Transports are wired to framings by the async loops:

```lean
let transport ← Async.block <|
  LeanWorker.Async.framedTransport byteTransport Framing.newline
```

The server and client then operate purely on `Transport (Except Error Json) Json`.
