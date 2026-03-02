# Framing

Framing is byte-oriented and independent of JSON-RPC semantics.

Implemented in:

- `LeanWorker/Framing/Newline.lean`
- `LeanWorker/Framing/ContentLength.lean`
- header helpers in `LeanWorker/Framing/Parse.lean`

Decoder shape:

```lean
ByteArray → Except JsonRpc.Error (Array ByteArray × ByteArray)
```

## Newline

- `Framing.encodeNewlineBytes`
- `Framing.decodeNewlineBytes`

Splits frames on trailing newline and returns remainder for partial input.

## Content-Length

- `Framing.encodeContentLengthBytes`
- `Framing.decodeContentLengthBytes`

Uses `Content-Length: <n>\r\n\r\n<body>`.

In stream mode, the runtime reads headers first, parses `Content-Length`, then reads body bytes until exactly `n` bytes are collected.

## Selecting Framing

`Transport.FrameSpec` currently supports:

- `.newline`
- `.contentLength`
