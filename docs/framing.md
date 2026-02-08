# Framing

Framing is byte-oriented. It does not parse JSON-RPC messages.

- Header parsing helpers: `LeanWorker/Framing/Parse.lean`
- Newline framing: `LeanWorker/Framing/Newline.lean`
- Content-Length framing: `LeanWorker/Framing/ContentLength.lean`
- HTTP-like framing: `LeanWorker/Framing/HttpLike.lean`

Each decoder returns complete payloads plus a remainder buffer:

```lean
ByteArray → Except JsonRpc.Error (Array ByteArray × ByteArray)
```

## Newline

- `Framing.encodeNewlineBytes : ByteArray → ByteArray`
- `Framing.decodeNewlineBytes : ByteArray → Except Error (Array ByteArray × ByteArray)`

Splits on newline and keeps partial trailing input as remainder.

## Content-Length

- `Framing.encodeContentLengthBytes : ByteArray → ByteArray`
- `Framing.decodeContentLengthBytes : ByteArray → Except Error (Array ByteArray × ByteArray)`

Uses `Content-Length: <n>\r\n\r\n<body>` framing.

## HTTP-like

- `Framing.HttpLikeConfig`
- `Framing.encodeHttpLikeBytes : HttpLikeConfig → ByteArray → ByteArray`
- `Framing.decodeHttpLikeBytes : ByteArray → Except Error (Array ByteArray × ByteArray)`

Adds start line + headers and still uses `Content-Length` for body boundaries.

## How It Is Used

You usually choose framing through `Transport.FrameSpec` when constructing a transport:

```lean
open LeanWorker

let transport ← Async.block <|
  Transport.jsonTransportFromStreams stdin stdout (.httpLike {}) log
```
