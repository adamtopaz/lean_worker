# Transport Abstraction

Transports are defined in `LeanWorker/Transport/Types.lean`.

```lean
structure Transport (Incoming Outgoing : Type) where
  inbox : Std.CloseableChannel Incoming
  outbox : Std.CloseableChannel Outgoing
  log : LogLevel → String → IO Unit
  shutdown : Async Unit
```

## Core Model

- The public API is typed transports only (no public byte transport alias).
- Framing is selected via `Transport.FrameSpec`.
- Payload encoding/decoding is handled by `Transport.Codec`.
- Protocol modules (`Streams`, `Tcp`, `Spawn`) construct typed transports directly.

## Codec

`LeanWorker/Transport/Codec.lean`:

```lean
structure Codec (Incoming Outgoing : Type) where
  decode : ByteArray → Except JsonRpc.Error Incoming
  encode : Outgoing → ByteArray
```

JSON codec: `LeanWorker/JsonRpc/Codec.lean` (`JsonRpc.jsonCodec`).

## Framing Selection

`LeanWorker/Transport/Streams.lean`:

```lean
inductive FrameSpec where
  | newline
  | contentLength
  | httpLike (config : Framing.HttpLikeConfig := {})
```

## Main Constructors

- `Transport.transportFromStreams`: streams + frame spec + codec -> typed transport.
- `Transport.jsonTransportFromStreams`: streams + frame spec -> JSON transport.
- `Transport.spawnStdioTransportWithCodec`: spawn stdio child with custom codec.
- `Transport.spawnStdioTransport`: spawn stdio child with JSON codec.
- `Transport.Tcp.connectTransport` / `Transport.Tcp.listenTransport`: typed TCP + codec.
- `Transport.Tcp.connectJsonTransport` / `Transport.Tcp.listenJsonTransport`: typed TCP JSON helpers.

## Example

```lean
open LeanWorker

def connectJsonOverStreams (readStream writeStream : IO.FS.Stream) : IO (Transport.Transport (Except JsonRpc.Error Lean.Json) Lean.Json) := do
  let log ← Transport.stderrLogger "CLIENT"
  Async.block <|
    Transport.jsonTransportFromStreams
      readStream
      writeStream
      .contentLength
      log
```
