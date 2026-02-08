module

public import LeanWorker.Transport.Logging
public import LeanWorker.Transport.Streams
public import LeanWorker.Async.Loops
public import Std.Internal.Async.TCP
public import Std.Sync.Channel

public section

namespace LeanWorker
namespace Transport
namespace Tcp

open Std.Internal.IO.Async
open Std.Internal.IO.Async.TCP
open Std.Net

private abbrev ByteTransport := LeanWorker.Transport.Transport ByteArray ByteArray

abbrev logLevelTag : LogLevel → String := Transport.logLevelTag

abbrev stderrLogger (label : String) : IO (LogLevel → String → IO Unit) :=
  Transport.stderrLogger label

abbrev silentLogger : LogLevel → String → IO Unit := Transport.silentLogger

def defaultLogger : IO (LogLevel → String → IO Unit) :=
  stderrLogger "TCP"

def defaultReadSize : UInt64 := 4096

private partial def clientReadLoop
    (client : TCP.Socket.Client)
    (log : LogLevel → String → IO Unit)
    (inbox : Std.CloseableChannel ByteArray) : Async Unit := do
  let chunk? ← TCP.Socket.Client.recv? client defaultReadSize
  match chunk? with
  | none =>
    LeanWorker.Async.closeOrLog log "tcp inbox" inbox
  | some chunk =>
    let sent ← LeanWorker.Async.sendOrLog log "tcp inbox" inbox chunk
    if sent then
      clientReadLoop client log inbox

private partial def clientWriteLoop
    (client : TCP.Socket.Client)
    (log : LogLevel → String → IO Unit)
    (outbox : Std.CloseableChannel ByteArray) : Async Unit := do
  match ← await <| ← outbox.recv with
  | none =>
    try
      TCP.Socket.Client.shutdown client
    catch err =>
      LeanWorker.Async.logError log s!"tcp shutdown error: {err}"
  | some bytes =>
    try
      TCP.Socket.Client.send client bytes
      clientWriteLoop client log outbox
    catch err =>
      LeanWorker.Async.logError log s!"tcp send error: {err}"
      LeanWorker.Async.closeOrLog log "tcp outbox" outbox

private def byteTransportFromClient
    (client : TCP.Socket.Client)
    (log : LogLevel → String → IO Unit) : Async ByteTransport := do
  let inbox : Std.CloseableChannel ByteArray ← Std.CloseableChannel.new
  let outbox : Std.CloseableChannel ByteArray ← Std.CloseableChannel.new
  let readLoop : Async Unit := do
    try
      clientReadLoop client log inbox
    catch err =>
      LeanWorker.Async.logError log s!"tcp read error: {err}"
      LeanWorker.Async.closeOrLog log "tcp inbox" inbox
  let writeLoop : Async Unit := do
    try
      clientWriteLoop client log outbox
    catch err =>
      LeanWorker.Async.logError log s!"tcp write error: {err}"
      LeanWorker.Async.closeOrLog log "tcp outbox" outbox
  let readerTask : AsyncTask Unit ← async readLoop
  let writerTask : AsyncTask Unit ← async writeLoop
  let shutdown : Async Unit := do
    LeanWorker.Async.closeOrLog log "tcp outbox" outbox
    try
      TCP.Socket.Client.shutdown client
    catch err =>
      LeanWorker.Async.logError log s!"tcp shutdown error: {err}"
    await readerTask
    await writerTask
  return { inbox, outbox, log, shutdown }

private def connectRawByteTransport
    (addr : SocketAddress)
    (log : LogLevel → String → IO Unit := silentLogger) : Async ByteTransport := do
  let client ← TCP.Socket.Client.mk
  TCP.Socket.Client.connect client addr
  byteTransportFromClient client log

private def transportFromRawByteTransport
    (rawTransport : ByteTransport)
    (frameSpec : FrameSpec)
    (codec : Codec Incoming Outgoing) :
    Async (Transport.Transport (Except JsonRpc.Error Incoming) Outgoing) :=
  Async.framedCodecTransport
    rawTransport
    (Transport.encodeFrame frameSpec)
    (Transport.decodeFrame frameSpec)
    codec

def connectTransport
    (addr : SocketAddress)
    (frameSpec : FrameSpec)
    (codec : Codec Incoming Outgoing)
    (log : LogLevel → String → IO Unit := silentLogger) :
    Async (Transport.Transport (Except JsonRpc.Error Incoming) Outgoing) := do
  let rawTransport ← connectRawByteTransport addr (log := log)
  transportFromRawByteTransport rawTransport frameSpec codec

def connectJsonTransport
    (addr : SocketAddress)
    (frameSpec : FrameSpec := .contentLength)
    (log : LogLevel → String → IO Unit := silentLogger) :
    Async (Transport.Transport (Except JsonRpc.Error Lean.Json) Lean.Json) := do
  let rawTransport ← connectRawByteTransport addr (log := log)
  transportFromRawByteTransport rawTransport frameSpec (JsonRpc.jsonCodec : Codec Lean.Json Lean.Json)

structure Listener where
  shutdown : Async Unit

private partial def acceptLoop
    (server : TCP.Socket.Server)
    (stopRef : IO.Ref Bool)
    (log : LogLevel → String → IO Unit)
    (handle : ByteTransport → Async Unit) : Async Unit := do
  if ← stopRef.get then
    return
  try
    match ← TCP.Socket.Server.tryAccept server with
    | some client =>
      let transport ← byteTransportFromClient client log
      discard <| async <| handle transport
      acceptLoop server stopRef log handle
    | none =>
      IO.sleep 10
      acceptLoop server stopRef log handle
  catch err =>
    LeanWorker.Async.logError log s!"tcp accept error: {err}"
    IO.sleep 10
    acceptLoop server stopRef log handle

private def listenRawByteTransport
    (addr : SocketAddress)
    (handle : ByteTransport → Async Unit)
    (backlog : UInt32 := 128)
    (log : LogLevel → String → IO Unit := silentLogger) : Async Listener := do
  let server ← TCP.Socket.Server.mk
  TCP.Socket.Server.bind server addr
  TCP.Socket.Server.listen server backlog
  let stopRef ← IO.mkRef false
  let task ← async <| acceptLoop server stopRef log handle
  let shutdown : Async Unit := do
    stopRef.set true
    await task
  return { shutdown }

def listenTransport
    (addr : SocketAddress)
    (handle : Transport.Transport (Except JsonRpc.Error Incoming) Outgoing → Async Unit)
    (frameSpec : FrameSpec)
    (codec : Codec Incoming Outgoing)
    (backlog : UInt32 := 128)
    (log : LogLevel → String → IO Unit := silentLogger) : Async Listener :=
  listenRawByteTransport addr
    (backlog := backlog)
    (log := log)
    (handle := fun rawTransport => do
      let transport ← transportFromRawByteTransport rawTransport frameSpec codec
      handle transport)

def listenJsonTransport
    (addr : SocketAddress)
    (handle : Transport.Transport (Except JsonRpc.Error Lean.Json) Lean.Json → Async Unit)
    (frameSpec : FrameSpec := .contentLength)
    (backlog : UInt32 := 128)
    (log : LogLevel → String → IO Unit := silentLogger) : Async Listener :=
  listenRawByteTransport addr
    (backlog := backlog)
    (log := log)
    (handle := fun rawTransport => do
      let transport ←
        transportFromRawByteTransport
          rawTransport
          frameSpec
          (JsonRpc.jsonCodec : Codec Lean.Json Lean.Json)
      handle transport)

end Tcp
end Transport
end LeanWorker
