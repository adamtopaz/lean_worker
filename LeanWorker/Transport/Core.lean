module

public import LeanWorker.Transport.Logging
public import LeanWorker.Framing
public import LeanWorker.JsonRpc.Parse
public import Std.Internal.Async.Basic

public section

namespace LeanWorker
namespace Transport

open Lean
open JsonRpc
open Std.Internal.IO.Async

private def readChunkSize : Nat := 4096

structure ByteSource where
  recv? : Nat → Async (Option ByteArray)

structure ByteSink where
  send : ByteArray → Async Unit
  flush : Async Unit := pure ()
  shutdown : Async Unit := pure ()

private def decodeJsonPayload (payload : ByteArray) : Except Error Json := do
  let text ←
    match String.fromUTF8? payload with
    | some text => Except.ok text
    | none =>
      throw <| Error.withData Error.parseError (Json.str "invalid UTF-8 in JSON payload")
  parseJson text

private def encodeJsonPayload (json : Json) : ByteArray :=
  (Json.compress json).toUTF8

private def writeFramedJson
    (sink : ByteSink)
    (codec : Framing.Codec)
    (json : Json) : Async Unit := do
  let payload := encodeJsonPayload json
  let bytes := codec.encode payload
  sink.send bytes
  sink.flush

private def emitPayloads
    (log : LogLevel → String → IO Unit)
    (inbox : Std.CloseableChannel (Except Error Json))
    (payloads : Array ByteArray) : Async Bool := do
  let mut continueLoop := true
  for payload in payloads do
    if continueLoop then
      match decodeJsonPayload payload with
      | .ok json =>
        let sent ← LeanWorker.Transport.sendOrLog log "json inbox" inbox (.ok json)
        if !sent then
          continueLoop := false
      | .error err =>
        LeanWorker.Transport.logError log
          s!"json decode error: {LeanWorker.Transport.errorToString err}"
        let sent ← LeanWorker.Transport.sendOrLog log "json inbox" inbox (.error err)
        if !sent then
          continueLoop := false
  return continueLoop

private partial def readJsonLoop
    (source : ByteSource)
    (codec : Framing.Codec)
    (log : LogLevel → String → IO Unit)
    (inbox : Std.CloseableChannel (Except Error Json))
    (buffer : ByteArray := ByteArray.empty) : Async Unit := do
  match ← source.recv? readChunkSize with
  | none =>
    if buffer.isEmpty then
      LeanWorker.Transport.closeOrLog log "json inbox" inbox
    else
      let sent ← LeanWorker.Transport.sendOrLog log "json inbox" inbox (.error codec.eofError)
      if sent then
        LeanWorker.Transport.closeOrLog log "json inbox" inbox
  | some chunk =>
    match codec.decode (buffer ++ chunk) with
    | .error err =>
      LeanWorker.Transport.logError log
        s!"framing decode error: {LeanWorker.Transport.errorToString err}"
      let sent ← LeanWorker.Transport.sendOrLog log "json inbox" inbox (.error err)
      if sent then
        readJsonLoop source codec log inbox ByteArray.empty
    | .ok (payloads, rest) =>
      let keepReading ← emitPayloads log inbox payloads
      if keepReading then
        readJsonLoop source codec log inbox rest

private partial def writeJsonLoop
    (sink : ByteSink)
    (codec : Framing.Codec)
    (outbox : Std.CloseableChannel Json) : Async Unit := do
  match ← await <| ← outbox.recv with
  | none =>
    sink.flush
    sink.shutdown
  | some json =>
    writeFramedJson sink codec json
    writeJsonLoop sink codec outbox

private def transportFromByteStreamsCore
    (source : ByteSource)
    (sink : ByteSink)
    (framing : Framing.Spec)
    (log : LogLevel → String → IO Unit) : Async Transport := do
  let inbox : Std.CloseableChannel (Except Error Json) ← Std.CloseableChannel.new
  let outbox : Std.CloseableChannel Json ← Std.CloseableChannel.new
  let codec := Framing.codec framing

  let _readerTask : AsyncTask Unit ← async do
    try
      readJsonLoop source codec log inbox
    catch err =>
      LeanWorker.Transport.logError log s!"json read task error: {err}"
      LeanWorker.Transport.closeOrLog log "json inbox" inbox

  let _writerTask : AsyncTask Unit ← async do
    try
      writeJsonLoop sink codec outbox
    catch err =>
      LeanWorker.Transport.logError log s!"json write task error: {err}"
      LeanWorker.Transport.closeOrLog log "json outbox" outbox
      LeanWorker.Transport.closeOrLog log "json inbox" inbox

  return { inbox, outbox, log }

def transportFromByteStreams
    (source : ByteSource)
    (sink : ByteSink)
    (framing : Framing.Spec := .newline)
    (log : LogLevel → String → IO Unit := silentLogger) : Async Transport :=
  transportFromByteStreamsCore source sink framing log

end Transport
end LeanWorker
