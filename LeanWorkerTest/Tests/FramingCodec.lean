module

public import LeanWorker
public import LeanWorkerTest.Tests.Support

public section

namespace LeanWorkerTest

open Lean
open LeanWorker
open LeanWorker.JsonRpc

def expectParseError (result : Except Error α) : IO Unit :=
  match result with
  | .ok _ =>
    throw <| IO.userError "expected parse error"
  | .error err =>
    assert (err.code == Error.parseError.code) s!"unexpected error code: {err.code}"

def testNewlineFramingRoundTrip : IO Unit := do
  let payload := "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}".toUTF8
  let wire := Framing.encodeNewlineBytes payload
  match Framing.decodeNewlineBytes wire with
  | .ok (payloads, rest) =>
    assert (payloads == #[payload]) "newline framing failed to round-trip payload"
    assert (rest.size == 0) "newline framing left unexpected remainder"
  | .error err =>
    throw <| IO.userError s!"newline decode failed: {err.code}"

def testNewlineFramingMultipleFrames : IO Unit := do
  let p1 := "{\"id\":1}".toUTF8
  let p2 := "{\"id\":2}".toUTF8
  let wire := Framing.encodeNewlineBytes p1 ++ Framing.encodeNewlineBytes p2
  match Framing.decodeNewlineBytes wire with
  | .ok (payloads, rest) =>
    assert (payloads == #[p1, p2]) "newline framing did not decode both payloads"
    assert (rest.size == 0) "newline framing left trailing bytes"
  | .error err =>
    throw <| IO.userError s!"newline decode failed: {err.code}"

def testNewlineFramingPartialFrame : IO Unit := do
  let partialPayload := "{\"jsonrpc\":\"2.0\"".toUTF8
  match Framing.decodeNewlineBytes partialPayload with
  | .ok (payloads, rest) =>
    assert payloads.isEmpty "newline decoder emitted payload for partial frame"
    assert (rest == partialPayload) "newline decoder did not preserve partial remainder"
  | .error err =>
    throw <| IO.userError s!"newline partial decode failed: {err.code}"

def testContentLengthFramingRoundTrip : IO Unit := do
  let payload := "{\"kind\":\"content-length\"}".toUTF8
  let wire := Framing.encodeContentLengthBytes payload
  match Framing.decodeContentLengthBytes wire with
  | .ok (payloads, rest) =>
    assert (payloads == #[payload]) "content-length framing failed to round-trip payload"
    assert (rest.size == 0) "content-length framing left unexpected remainder"
  | .error err =>
    throw <| IO.userError s!"content-length decode failed: {err.code}"

def testContentLengthFramingPartialSecondFrame : IO Unit := do
  let p1 := "{\"a\":1}".toUTF8
  let p2 := "{\"b\":2}".toUTF8
  let frame1 := Framing.encodeContentLengthBytes p1
  let frame2 := Framing.encodeContentLengthBytes p2
  let partialFrame2 := frame2.extract 0 (frame2.size - 4)
  let wire := frame1 ++ partialFrame2
  match Framing.decodeContentLengthBytes wire with
  | .ok (payloads, rest) =>
    assert (payloads == #[p1]) "content-length decoder should emit only the complete frame"
    assert (rest == partialFrame2) "content-length decoder lost partial trailing frame"
  | .error err =>
    throw <| IO.userError s!"content-length partial decode failed: {err.code}"

def testContentLengthFramingMissingHeader : IO Unit := do
  let wire := "X-Test: 1\r\n\r\n{}".toUTF8
  expectParseError (Framing.decodeContentLengthBytes wire)

def testHttpLikeFramingRoundTrip : IO Unit := do
  let payload := "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"ok\"}".toUTF8
  let wire := Framing.encodeHttpLikeBytes {} payload
  match Framing.decodeHttpLikeBytes wire with
  | .ok (payloads, rest) =>
    assert (payloads == #[payload]) "http-like framing failed to round-trip payload"
    assert (rest.size == 0) "http-like framing left unexpected remainder"
  | .error err =>
    throw <| IO.userError s!"http-like decode failed: {err.code}"

def testHttpLikeFramingMissingStartLine : IO Unit := do
  let wire := "\r\nContent-Length: 2\r\n\r\n{}".toUTF8
  expectParseError (Framing.decodeHttpLikeBytes wire)

def testJsonCodecRoundTrip : IO Unit := do
  let json := Json.mkObj [
    ("name", Json.str "codec"),
    ("count", Json.num 3),
    ("ok", Json.bool true)
  ]
  let payload := JsonRpc.jsonCodec.encode json
  match JsonRpc.jsonCodec.decode payload with
  | .ok decoded =>
    assert (decoded == json) "json codec failed to round-trip json payload"
  | .error err =>
    throw <| IO.userError s!"json codec decode failed: {err.code}"

def testJsonCodecInvalidUtf8 : IO Unit := do
  let invalid : ByteArray := ByteArray.mk #[0xFF, 0xFE]
  expectParseError (JsonRpc.jsonCodec.decode invalid)

def testJsonCodecInvalidJson : IO Unit := do
  let invalid := "{bad json".toUTF8
  expectParseError (JsonRpc.jsonCodec.decode invalid)

def testContentLengthFramingWithJsonCodec : IO Unit := do
  let j1 := Json.mkObj [("kind", Json.str "one")]
  let j2 := Json.mkObj [("kind", Json.str "two")]
  let p1 := JsonRpc.jsonCodec.encode j1
  let p2 := JsonRpc.jsonCodec.encode j2
  let wire := Framing.encodeContentLengthBytes p1 ++ Framing.encodeContentLengthBytes p2
  match Framing.decodeContentLengthBytes wire with
  | .ok (payloads, rest) =>
    assert (rest.size == 0) "content-length/json codec test left unexpected remainder"
    let decoded ←
      match payloads.mapM JsonRpc.jsonCodec.decode with
      | .ok values => pure values
      | .error err => throw <| IO.userError s!"json codec decode failed: {err.code}"
    assert (decoded == #[j1, j2]) "content-length/json codec decode mismatch"
  | .error err =>
    throw <| IO.userError s!"content-length framing decode failed: {err.code}"

def testContentLengthFramingWithJsonCodecInvalidPayload : IO Unit := do
  let wire := Framing.encodeContentLengthBytes "{bad".toUTF8
  match Framing.decodeContentLengthBytes wire with
  | .error err =>
    throw <| IO.userError s!"unexpected framing error: {err.code}"
  | .ok (payloads, rest) =>
    assert (rest.size == 0) "invalid payload test left unexpected remainder"
    match payloads.mapM JsonRpc.jsonCodec.decode with
    | .ok _ =>
      throw <| IO.userError "expected codec parse error for invalid json payload"
    | .error err =>
      assert (err.code == Error.parseError.code) s!"unexpected codec error code: {err.code}"

end LeanWorkerTest
