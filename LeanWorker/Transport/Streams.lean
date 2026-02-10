module

public import LeanWorker.Transport.Types
public import LeanWorker.Transport.Codec
public import LeanWorker.Async.Loops
public import LeanWorker.Framing.Newline
public import LeanWorker.Framing.ContentLength
public import LeanWorker.Framing.HttpLike
public import LeanWorker.JsonRpc.Codec
public import Std.Internal.Async.Basic

public section

namespace LeanWorker
namespace Transport

open JsonRpc
open Std.Internal.IO.Async

private abbrev RawByteTransport := Transport ByteArray ByteArray

private def readChunkSize : Nat := 4096

inductive FrameSpec where
  | newline
  | contentLength
  | httpLike (config : Framing.HttpLikeConfig := {})
deriving Inhabited

def encodeFrame (frameSpec : FrameSpec) : ByteArray → ByteArray :=
  match frameSpec with
  | .newline => Framing.encodeNewlineBytes
  | .contentLength => Framing.encodeContentLengthBytes
  | .httpLike config => Framing.encodeHttpLikeBytes config

def decodeFrame
    (frameSpec : FrameSpec) :
    ByteArray → Except JsonRpc.Error (Array ByteArray × ByteArray) :=
  match frameSpec with
  | .newline => Framing.decodeNewlineBytes
  | .contentLength => Framing.decodeContentLengthBytes
  | .httpLike _ => Framing.decodeHttpLikeBytes

private partial def streamReadLoop
    (readStream : IO.FS.Stream)
    (log : LogLevel → String → IO Unit)
    (inbox : Std.CloseableChannel ByteArray)
    (readSize : USize := USize.ofNat 4096) : Async Unit := do
  let chunk ← readStream.read readSize
  if chunk.size == 0 then
    LeanWorker.Async.closeOrLog log "byte inbox" inbox
  else
    let sent ← LeanWorker.Async.sendOrLog log "byte inbox" inbox chunk
    if sent then
      streamReadLoop readStream log inbox readSize

private partial def lineReadLoop
    (readStream : IO.FS.Stream)
    (log : LogLevel → String → IO Unit)
    (inbox : Std.CloseableChannel ByteArray) : Async Unit := do
  let line := (← readStream.getLine)
  if line.isEmpty then
    LeanWorker.Async.closeOrLog log "byte inbox" inbox
  else
    let sent ← LeanWorker.Async.sendOrLog log "byte inbox" inbox line.toUTF8
    if sent then
      lineReadLoop readStream log inbox

private def parseContentLengthFromHeaderLines
    (headerLines : List String) : Except JsonRpc.Error Nat := do
  let headers ← headerLines.mapM Framing.parseHeaderLine
  Framing.parseContentLength headers

private def parseHttpLikeContentLengthFromLines
    (startLine : String)
    (headerLines : List String) : Except JsonRpc.Error Nat := do
  if startLine.trimAscii.isEmpty then
    throw <| Framing.framingError "missing start line"
  let headers ← headerLines.mapM Framing.parseHeaderLine
  Framing.parseContentLength headers

private partial def readHeaderLines
    (readStream : IO.FS.Stream)
    (log : LogLevel → String → IO Unit)
    (inbox : Std.CloseableChannel ByteArray)
    (acc : List String := [])
    (sawBytes : Bool := false) : Async (Except JsonRpc.Error (Option (List String))) := do
  let line := (← readStream.getLine)
  if line.isEmpty then
    if sawBytes then
      return .error <| Framing.framingError "unexpected EOF while reading headers"
    else
      return .ok none
  let sent ← LeanWorker.Async.sendOrLog log "byte inbox" inbox line.toUTF8
  if !sent then
    return .ok none
  let normalized := line.trimAscii.toString
  if normalized.isEmpty then
    return .ok <| some acc.reverse
  else
    readHeaderLines readStream log inbox (normalized :: acc) true

private partial def readHttpLikeHeaderLines
    (readStream : IO.FS.Stream)
    (log : LogLevel → String → IO Unit)
    (inbox : Std.CloseableChannel ByteArray)
    (acc : List String := []) : Async (Except JsonRpc.Error (List String)) := do
  let line := (← readStream.getLine)
  if line.isEmpty then
    return .error <| Framing.framingError "unexpected EOF while reading headers"
  let sent ← LeanWorker.Async.sendOrLog log "byte inbox" inbox line.toUTF8
  if !sent then
    return .ok acc.reverse
  let normalized := line.trimAscii.toString
  if normalized.isEmpty then
    return .ok acc.reverse
  else
    readHttpLikeHeaderLines readStream log inbox (normalized :: acc)

private def readHttpLikeHeaders
    (readStream : IO.FS.Stream)
    (log : LogLevel → String → IO Unit)
    (inbox : Std.CloseableChannel ByteArray) : Async (Except JsonRpc.Error (Option (String × List String))) := do
  let startLineRaw := (← readStream.getLine)
  if startLineRaw.isEmpty then
    return .ok none
  let sent ← LeanWorker.Async.sendOrLog log "byte inbox" inbox startLineRaw.toUTF8
  if !sent then
    return .ok none
  let startLine := startLineRaw.trimAscii.toString
  match ← readHttpLikeHeaderLines readStream log inbox with
  | .error err =>
    return .error err
  | .ok headerLines =>
    return .ok <| some (startLine, headerLines)

private partial def readBodyBytes
    (readStream : IO.FS.Stream)
    (log : LogLevel → String → IO Unit)
    (inbox : Std.CloseableChannel ByteArray)
    (remaining : Nat) : Async Bool := do
  if remaining == 0 then
    return true
  let nextChunk := Nat.min remaining readChunkSize
  let chunk ← readStream.read (USize.ofNat nextChunk)
  if chunk.size == 0 then
    LeanWorker.Async.logError log "unexpected EOF while reading framed body"
    LeanWorker.Async.closeOrLog log "byte inbox" inbox
    return false
  let sent ← LeanWorker.Async.sendOrLog log "byte inbox" inbox chunk
  if sent then
    readBodyBytes readStream log inbox (remaining - chunk.size)
  else
    return false

private partial def contentLengthReadLoop
    (readStream : IO.FS.Stream)
    (log : LogLevel → String → IO Unit)
    (inbox : Std.CloseableChannel ByteArray) : Async Unit := do
  match ← readHeaderLines readStream log inbox with
  | .ok none =>
    LeanWorker.Async.closeOrLog log "byte inbox" inbox
  | .error err =>
    LeanWorker.Async.logError log s!"content-length framing read error: {LeanWorker.Async.errorToString err}"
    LeanWorker.Async.closeOrLog log "byte inbox" inbox
  | .ok (some headerLines) =>
    match parseContentLengthFromHeaderLines headerLines with
    | .error err =>
      LeanWorker.Async.logAsync log .debug s!"content-length framing parse error: {LeanWorker.Async.errorToString err}"
      LeanWorker.Async.closeOrLog log "byte inbox" inbox
    | .ok contentLength =>
      let ok ← readBodyBytes readStream log inbox contentLength
      if ok then
        contentLengthReadLoop readStream log inbox

private partial def httpLikeReadLoop
    (readStream : IO.FS.Stream)
    (log : LogLevel → String → IO Unit)
    (inbox : Std.CloseableChannel ByteArray) : Async Unit := do
  match ← readHttpLikeHeaders readStream log inbox with
  | .ok none =>
    LeanWorker.Async.closeOrLog log "byte inbox" inbox
  | .error err =>
    LeanWorker.Async.logError log s!"http-like framing read error: {LeanWorker.Async.errorToString err}"
    LeanWorker.Async.closeOrLog log "byte inbox" inbox
  | .ok (some (startLine, headerLines)) =>
    match parseHttpLikeContentLengthFromLines startLine headerLines with
    | .error err =>
      LeanWorker.Async.logAsync log .debug s!"http-like framing parse error: {LeanWorker.Async.errorToString err}"
      LeanWorker.Async.closeOrLog log "byte inbox" inbox
    | .ok contentLength =>
      let ok ← readBodyBytes readStream log inbox contentLength
      if ok then
        httpLikeReadLoop readStream log inbox

private partial def streamWriteLoop
    (writeStream : IO.FS.Stream)
    (log : LogLevel → String → IO Unit)
    (outbox : Std.CloseableChannel ByteArray) : Async Unit := do
  match ← await <| ← outbox.recv with
  | none => return
  | some bytes =>
    writeStream.write bytes
    writeStream.flush
    streamWriteLoop writeStream log outbox

private def rawByteTransportFromStreams
    (readStream writeStream : IO.FS.Stream)
    (log : LogLevel → String → IO Unit)
    (shutdownAction : Async Unit := pure ())
    (readSize : USize := USize.ofNat 4096) : Async RawByteTransport := do
  let inbox : Std.CloseableChannel ByteArray ← Std.CloseableChannel.new
  let outbox : Std.CloseableChannel ByteArray ← Std.CloseableChannel.new
  let readLoop : Async Unit := do
    try
      streamReadLoop readStream log inbox readSize
    catch err =>
      LeanWorker.Async.logError log s!"byte read error: {err}"
      LeanWorker.Async.closeOrLog log "byte inbox" inbox
  let writeLoop : Async Unit := do
    try
      streamWriteLoop writeStream log outbox
    catch err =>
      LeanWorker.Async.logError log s!"byte write error: {err}"
  let readerTask : AsyncTask Unit ← async readLoop
  let writerTask : AsyncTask Unit ← async writeLoop
  let shutdown : Async Unit := do
    await readerTask
    await writerTask
    shutdownAction
  return { inbox, outbox, log, shutdown }

private def rawLineTransportFromStreams
    (readStream writeStream : IO.FS.Stream)
    (log : LogLevel → String → IO Unit)
    (shutdownAction : Async Unit := pure ()) : Async RawByteTransport := do
  let inbox : Std.CloseableChannel ByteArray ← Std.CloseableChannel.new
  let outbox : Std.CloseableChannel ByteArray ← Std.CloseableChannel.new
  let readLoop : Async Unit := do
    try
      lineReadLoop readStream log inbox
    catch err =>
      LeanWorker.Async.logError log s!"byte read error: {err}"
      LeanWorker.Async.closeOrLog log "byte inbox" inbox
  let writeLoop : Async Unit := do
    try
      streamWriteLoop writeStream log outbox
    catch err =>
      LeanWorker.Async.logError log s!"byte write error: {err}"
  let readerTask : AsyncTask Unit ← async readLoop
  let writerTask : AsyncTask Unit ← async writeLoop
  let shutdown : Async Unit := do
    await readerTask
    await writerTask
    shutdownAction
  return { inbox, outbox, log, shutdown }

private def rawContentLengthTransportFromStreams
    (readStream writeStream : IO.FS.Stream)
    (log : LogLevel → String → IO Unit)
    (shutdownAction : Async Unit := pure ()) : Async RawByteTransport := do
  let inbox : Std.CloseableChannel ByteArray ← Std.CloseableChannel.new
  let outbox : Std.CloseableChannel ByteArray ← Std.CloseableChannel.new
  let readLoop : Async Unit := do
    try
      contentLengthReadLoop readStream log inbox
    catch err =>
      LeanWorker.Async.logError log s!"byte read error: {err}"
      LeanWorker.Async.closeOrLog log "byte inbox" inbox
  let writeLoop : Async Unit := do
    try
      streamWriteLoop writeStream log outbox
    catch err =>
      LeanWorker.Async.logError log s!"byte write error: {err}"
  let readerTask : AsyncTask Unit ← async readLoop
  let writerTask : AsyncTask Unit ← async writeLoop
  let shutdown : Async Unit := do
    await readerTask
    await writerTask
    shutdownAction
  return { inbox, outbox, log, shutdown }

private def rawHttpLikeTransportFromStreams
    (readStream writeStream : IO.FS.Stream)
    (log : LogLevel → String → IO Unit)
    (shutdownAction : Async Unit := pure ()) : Async RawByteTransport := do
  let inbox : Std.CloseableChannel ByteArray ← Std.CloseableChannel.new
  let outbox : Std.CloseableChannel ByteArray ← Std.CloseableChannel.new
  let readLoop : Async Unit := do
    try
      httpLikeReadLoop readStream log inbox
    catch err =>
      LeanWorker.Async.logError log s!"byte read error: {err}"
      LeanWorker.Async.closeOrLog log "byte inbox" inbox
  let writeLoop : Async Unit := do
    try
      streamWriteLoop writeStream log outbox
    catch err =>
      LeanWorker.Async.logError log s!"byte write error: {err}"
  let readerTask : AsyncTask Unit ← async readLoop
  let writerTask : AsyncTask Unit ← async writeLoop
  let shutdown : Async Unit := do
    await readerTask
    await writerTask
    shutdownAction
  return { inbox, outbox, log, shutdown }

private def transportFromRawByteTransport
    (rawTransport : RawByteTransport)
    (frameSpec : FrameSpec)
    (codec : Codec Incoming Outgoing) :
    Async (Transport (Except JsonRpc.Error Incoming) Outgoing) :=
  Async.framedCodecTransport
    rawTransport
    (encodeFrame frameSpec)
    (decodeFrame frameSpec)
    codec

def transportFromStreams
    (readStream writeStream : IO.FS.Stream)
    (frameSpec : FrameSpec)
    (codec : Codec Incoming Outgoing)
    (log : LogLevel → String → IO Unit)
    (shutdownAction : Async Unit := pure ()) :
    Async (Transport (Except JsonRpc.Error Incoming) Outgoing) := do
  let rawTransport ←
    match frameSpec with
    | .newline => rawLineTransportFromStreams readStream writeStream log shutdownAction
    | .contentLength => rawContentLengthTransportFromStreams readStream writeStream log shutdownAction
    | .httpLike _ => rawHttpLikeTransportFromStreams readStream writeStream log shutdownAction
  transportFromRawByteTransport rawTransport frameSpec codec

def jsonTransportFromStreams
    (readStream writeStream : IO.FS.Stream)
    (frameSpec : FrameSpec)
    (log : LogLevel → String → IO Unit)
    (shutdownAction : Async Unit := pure ()) :
    Async (Transport (Except JsonRpc.Error Lean.Json) Lean.Json) :=
  transportFromStreams
    readStream
    writeStream
    frameSpec
    (JsonRpc.jsonCodec : Codec Lean.Json Lean.Json)
    log
    shutdownAction

end Transport
end LeanWorker
