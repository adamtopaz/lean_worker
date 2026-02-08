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
    | .contentLength =>
      rawByteTransportFromStreams readStream writeStream log shutdownAction (USize.ofNat 1)
    | .httpLike _ =>
      rawByteTransportFromStreams readStream writeStream log shutdownAction (USize.ofNat 1)
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
