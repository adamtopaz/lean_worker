# Framing

Framing converts raw bytes to JSON values and back. It is defined in `LeanWorker/Framing/Types.lean` and implemented in:

- `LeanWorker/Framing/Newline.lean`
- `LeanWorker/Framing/ContentLength.lean`
- `LeanWorker/Framing/HttpLike.lean`

## Framing Type

```lean
structure Framing where
  encode : Json → ByteArray
  decode : ByteArray → Except Error (Array Json × ByteArray)
```

`decode` returns a list of JSON messages and the leftover buffer (for partial frames).

## Newline Framing

- Encode: `Json.compress` + `"\n"`.
- Decode: split on newline; each line is parsed as JSON.

## Content-Length Framing

- Encode: `Content-Length: <len>\r\n\r\n` + JSON body.
- Decode: parse headers, read `Content-Length`, parse body.

## HTTP-like Framing

- Adds a start line and HTTP-style headers.
- Still uses `Content-Length` and a JSON body.
- Useful for `curl` and basic HTTP clients, but it is not a full HTTP server.

## Example

```lean
open LeanWorker

def runServer : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let log ← Transport.stderrLogger "SERVER"
  let byteTransport ← Async.block <| Transport.byteTransportFromStreams stdin stdout log
  let transport ← Async.block <| Async.framedTransport byteTransport Framing.contentLength
  let state ← Std.Mutex.new 0
  Async.block <| Server.run server () state
```
