# Async Loops

Async loop utilities are defined in `LeanWorker/Async/Loops.lean`. They connect byte-level transports to JSON-level transports using a framing.

## readLoop

- Buffers incoming bytes.
- Uses `Framing.decode` to parse JSON values.
- For decode errors, logs the error and resets the buffer.
- Closes JSON inbox when byte inbox closes.

## writeLoop

- Encodes outgoing JSON using `Framing.encode`.
- Writes to the byte outbox.
- Closes the byte outbox when the JSON outbox closes.

## framedTransport

```lean
def framedTransport
    (byteTransport : ByteTransport)
    (framing : Framing) : Async (Transport (Except Error Json) Json)
```

Creates a JSON transport with fresh channels and spawns reader/writer tasks. The returned `shutdown` waits for those tasks and the underlying byte transport.

## Logging Helpers

- `sendOrLog` and `closeOrLog` log channel errors instead of crashing.
- `logError` and `logAsync` write via the transport logger (not stdout).
