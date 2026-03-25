module

import LeanWorker.Transport
import LeanWorker.Framing

namespace LeanWorkerTest

open Lean
open LeanWorker
open Transport
open Framing
open JsonRpc
open Std.Internal.IO.Async

private def encodeJsonPayload (json : Json) : ByteArray :=
  (Json.compress json).toUTF8

private def byteSourceOfChunks (chunksRef : IO.Ref (List ByteArray)) : ByteSource where
  recv? _ :=
    chunksRef.modifyGet fun chunks =>
      match chunks with
      | [] => (none, [])
      | chunk :: rest => (some chunk, rest)

private def nullByteSink : ByteSink where
  send _ := pure ()

private def recvInbox
    (transport : LeanWorker.Transport.Transport) : IO (Option (Except Error Json)) :=
  Async.block do
    await <| ← transport.inbox.recv

private def runContentLengthPrefixErrorTest : IO Unit := do
  let validJson := Json.mkObj [("kind", Json.str "ok")]
  let validFrame := Framing.encode .contentLength (encodeJsonPayload validJson)
  let malformedHeader := "Content-Length: nope\r\n\r\n".toUTF8
  let chunksRef ← IO.mkRef [validFrame ++ malformedHeader]
  let transport ← Async.block <|
    transportFromByteStreams (byteSourceOfChunks chunksRef) nullByteSink .contentLength silentLogger
  try
    match ← recvInbox transport with
    | some (.ok json) =>
      if json != validJson then
        throw <| IO.userError s!"unexpected first payload: {Json.compress json}"
    | some (.error err) =>
      throw <| IO.userError s!"expected first inbox item to be valid JSON, got error: {err.message}"
    | none =>
      throw <| IO.userError "expected first inbox item to be present"

    match ← recvInbox transport with
    | some (.error err) =>
      if err.code != Error.parseError.code then
        throw <| IO.userError s!"unexpected error code: {err.code}"
    | some (.ok json) =>
      throw <| IO.userError s!"expected framing error after first payload, got: {Json.compress json}"
    | none =>
      throw <| IO.userError "expected second inbox item to be a framing error"

    match ← recvInbox transport with
    | none =>
      pure ()
    | some (.ok json) =>
      throw <| IO.userError s!"expected inbox to be closed, got JSON: {Json.compress json}"
    | some (.error err) =>
      throw <| IO.userError s!"expected inbox to be closed, got error: {err.message}"
  finally
    Async.block <| LeanWorker.Transport.closeOrLog transport.log "test outbox" transport.outbox
    Async.block <| LeanWorker.Transport.closeOrLog transport.log "test inbox" transport.inbox

end LeanWorkerTest

public def main (_args : List String) : IO UInt32 := do
  try
    LeanWorkerTest.runContentLengthPrefixErrorTest
    return 0
  catch err =>
    let stderr ← IO.getStderr
    stderr.putStrLn s!"error: {err}"
    return 1
