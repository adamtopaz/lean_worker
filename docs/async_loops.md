# Async Loops

Async transport loop utilities live in `LeanWorker/Async/Loops.lean`.

The main bridge is:

```lean
partial def framedCodecTransport
    (rawTransport : Transport.Transport ByteArray ByteArray)
    (encodeFrame : ByteArray → ByteArray)
    (decodeFrame : ByteArray → Except JsonRpc.Error (Array ByteArray × ByteArray))
    (codec : Transport.Codec Incoming Outgoing) :
    Async (Transport (Except JsonRpc.Error Incoming) Outgoing)
```

## Behavior

- Reader loop buffers incoming bytes from the raw transport.
- It runs framing decode to recover payload byte arrays.
- It runs codec decode to recover typed incoming messages.
- Framing/codec decode failures are forwarded as `.error` on typed inbox.
- Writer loop codec-encodes outgoing values and applies framing encode before send.
- Shutdown waits for both loop tasks and the underlying raw transport shutdown.

## Logging Helpers

- `sendOrLog`: send to channel, log on channel errors.
- `closeOrLog`: close channel, log on channel errors.
- `logError` / `logAsync`: route operational logs through transport logger.

In normal usage you do not call this directly; `Transport.Streams`, `Transport.Tcp`, and `Transport.Spawn` use it internally.
