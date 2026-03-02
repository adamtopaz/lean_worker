module

public import LeanWorker.Transport.Types
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

inductive FrameSpec where
  | newline
  | contentLength
deriving BEq, Repr, Inhabited

private def encodeFrame (frameSpec : FrameSpec) : ByteArray → ByteArray :=
  match frameSpec with
  | .newline => Framing.encodeNewlineBytes
  | .contentLength => Framing.encodeContentLengthBytes

private def decodeFrame
    (frameSpec : FrameSpec) :
    ByteArray → Except Error (Array ByteArray × ByteArray) :=
  match frameSpec with
  | .newline => Framing.decodeNewlineBytes
  | .contentLength => Framing.decodeContentLengthBytes

private def decodeJsonPayload (payload : ByteArray) : Except Error Json := do
  let text ←
    match String.fromUTF8? payload with
    | some text => Except.ok text
    | none =>
      throw <| Error.withData Error.parseError (Json.str "invalid UTF-8 in JSON payload")
  parseJson text

private def encodeJsonPayload (json : Json) : ByteArray :=
  (Json.compress json).toUTF8

private def eofFramingError (frameSpec : FrameSpec) : Error :=
  match frameSpec with
  | .newline => Framing.framingError "unexpected EOF while reading newline-framed payload"
  | .contentLength => Framing.framingError "unexpected EOF while reading content-length-framed payload"

private def writeFramedJson
    (writeStream : IO.FS.Stream)
    (frameSpec : FrameSpec)
    (json : Json) : IO Unit := do
  let payload := encodeJsonPayload json
  let bytes := encodeFrame frameSpec payload
  writeStream.write bytes
  writeStream.flush

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

private def parseContentLengthFromHeaderLines
    (headerLines : List String) : Except Error Nat := do
  let headers ← headerLines.mapM Framing.parseHeaderLine
  Framing.parseContentLength headers

private partial def readHeaderLines
    (readStream : IO.FS.Stream)
    (acc : List String := [])
    (sawBytes : Bool := false) : Async (Except Error (Option (List String))) := do
  let line := (← readStream.getLine)
  if line.isEmpty then
    if sawBytes then
      return .error <| Framing.framingError "unexpected EOF while reading headers"
    else
      return .ok none
  let normalized := line.trimAscii.toString
  if normalized.isEmpty then
    return .ok <| some acc.reverse
  else
    readHeaderLines readStream (normalized :: acc) true

private partial def readBodyBytes
    (readStream : IO.FS.Stream)
    (remaining : Nat)
    (buffer : ByteArray := ByteArray.empty) : Async (Except Error ByteArray) := do
  if remaining == 0 then
    return .ok buffer
  let nextChunk := Nat.min remaining readChunkSize
  let chunk ← readStream.read (USize.ofNat nextChunk)
  if chunk.size == 0 then
    return .error <| Framing.framingError "unexpected EOF while reading framed body"
  readBodyBytes readStream (remaining - chunk.size) (buffer ++ chunk)

private partial def readNewlineLoop
    (readStream : IO.FS.Stream)
    (log : LogLevel → String → IO Unit)
    (inbox : Std.CloseableChannel (Except Error Json))
    (buffer : ByteArray := ByteArray.empty) : Async Unit := do
  let line := (← readStream.getLine)
  if line.isEmpty then
    if buffer.isEmpty then
      LeanWorker.Transport.closeOrLog log "json inbox" inbox
    else
      let err := eofFramingError .newline
      let sent ← LeanWorker.Transport.sendOrLog log "json inbox" inbox (.error err)
      if sent then
        LeanWorker.Transport.closeOrLog log "json inbox" inbox
  else
    let buffer := buffer ++ line.toUTF8
    match decodeFrame .newline buffer with
    | .error err =>
      LeanWorker.Transport.logError log
        s!"framing decode error: {LeanWorker.Transport.errorToString err}"
      let sent ← LeanWorker.Transport.sendOrLog log "json inbox" inbox (.error err)
      if sent then
        readNewlineLoop readStream log inbox ByteArray.empty
    | .ok (payloads, rest) =>
      let keepReading ← emitPayloads log inbox payloads
      if keepReading then
        readNewlineLoop readStream log inbox rest

private partial def readContentLengthLoop
    (readStream : IO.FS.Stream)
    (log : LogLevel → String → IO Unit)
    (inbox : Std.CloseableChannel (Except Error Json)) : Async Unit := do
  match ← readHeaderLines readStream with
  | .ok none =>
    LeanWorker.Transport.closeOrLog log "json inbox" inbox
  | .error err =>
    LeanWorker.Transport.logError log
      s!"framing decode error: {LeanWorker.Transport.errorToString err}"
    let sent ← LeanWorker.Transport.sendOrLog log "json inbox" inbox (.error err)
    if sent then
      LeanWorker.Transport.closeOrLog log "json inbox" inbox
  | .ok (some headerLines) =>
    match parseContentLengthFromHeaderLines headerLines with
    | .error err =>
      LeanWorker.Transport.logError log
        s!"framing decode error: {LeanWorker.Transport.errorToString err}"
      let sent ← LeanWorker.Transport.sendOrLog log "json inbox" inbox (.error err)
      if sent then
        LeanWorker.Transport.closeOrLog log "json inbox" inbox
    | .ok contentLength =>
      match ← readBodyBytes readStream contentLength with
      | .error err =>
        LeanWorker.Transport.logError log
          s!"framing decode error: {LeanWorker.Transport.errorToString err}"
        let sent ← LeanWorker.Transport.sendOrLog log "json inbox" inbox (.error err)
        if sent then
          LeanWorker.Transport.closeOrLog log "json inbox" inbox
      | .ok payload =>
        let keepReading ← emitPayloads log inbox #[payload]
        if keepReading then
          readContentLengthLoop readStream log inbox

private def readJsonLoop
    (readStream : IO.FS.Stream)
    (frameSpec : FrameSpec)
    (log : LogLevel → String → IO Unit)
    (inbox : Std.CloseableChannel (Except Error Json)) : Async Unit :=
  match frameSpec with
  | .newline => readNewlineLoop readStream log inbox
  | .contentLength => readContentLengthLoop readStream log inbox

private partial def writeJsonLoop
    (writeStream : IO.FS.Stream)
    (frameSpec : FrameSpec)
    (outbox : Std.CloseableChannel Json) : Async Unit := do
  match ← await <| ← outbox.recv with
  | none =>
    return
  | some json =>
    writeFramedJson writeStream frameSpec json
    writeJsonLoop writeStream frameSpec outbox

private def transportFromStreamsCore
    (readStream writeStream : IO.FS.Stream)
    (frameSpec : FrameSpec)
    (log : LogLevel → String → IO Unit) : Async Transport := do
  let inbox : Std.CloseableChannel (Except Error Json) ← Std.CloseableChannel.new
  let outbox : Std.CloseableChannel Json ← Std.CloseableChannel.new

  let _readerTask : AsyncTask Unit ← async do
    try
      readJsonLoop readStream frameSpec log inbox
    catch err =>
      LeanWorker.Transport.logError log s!"json read task error: {err}"
      LeanWorker.Transport.closeOrLog log "json inbox" inbox

  let _writerTask : AsyncTask Unit ← async do
    try
      writeJsonLoop writeStream frameSpec outbox
    catch err =>
      LeanWorker.Transport.logError log s!"json write task error: {err}"
      LeanWorker.Transport.closeOrLog log "json outbox" outbox
      LeanWorker.Transport.closeOrLog log "json inbox" inbox

  return { inbox, outbox, log }

def transportFromStreams
    (readStream writeStream : IO.FS.Stream)
    (frameSpec : FrameSpec := .newline)
    (log : LogLevel → String → IO Unit := silentLogger) : Async Transport :=
  transportFromStreamsCore readStream writeStream frameSpec log

def serverTransportFromStreams
    (readStream writeStream : IO.FS.Stream)
    (frameSpec : FrameSpec := .newline)
    (log : LogLevel → String → IO Unit := silentLogger) : Async Transport :=
  transportFromStreams readStream writeStream frameSpec log

def clientTransportFromStreams
    (readStream writeStream : IO.FS.Stream)
    (frameSpec : FrameSpec := .newline)
    (log : LogLevel → String → IO Unit := silentLogger) : Async Transport :=
  transportFromStreams readStream writeStream frameSpec log

def serverTransportFromStdio
    (frameSpec : FrameSpec := .newline)
    (log : LogLevel → String → IO Unit := silentLogger) : Async Transport := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  serverTransportFromStreams stdin stdout frameSpec log

def clientTransportFromStdio
    (frameSpec : FrameSpec := .newline)
    (log : LogLevel → String → IO Unit := silentLogger) : Async Transport := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  clientTransportFromStreams stdin stdout frameSpec log

end Transport
end LeanWorker
