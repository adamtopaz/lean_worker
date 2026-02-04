module

public import LeanWorker.Transport.Types
public import LeanWorker.Async.Loops
public import Std.Internal.Async.Basic

public section

namespace LeanWorker
namespace Transport

open Std.Internal.IO.Async

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

private partial def lineWriteLoop
    (writeStream : IO.FS.Stream)
    (log : LogLevel → String → IO Unit)
    (outbox : Std.CloseableChannel ByteArray) : Async Unit := do
  match ← await <| ← outbox.recv with
  | none => return
  | some bytes =>
    writeStream.write bytes
    writeStream.flush
    lineWriteLoop writeStream log outbox

def lineByteTransportFromStreams
    (readStream writeStream : IO.FS.Stream)
    (log : LogLevel → String → IO Unit)
    (shutdownAction : Async Unit := pure ()) : Async ByteTransport := do
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
      lineWriteLoop writeStream log outbox
    catch err =>
      LeanWorker.Async.logError log s!"byte write error: {err}"
  let readerTask : AsyncTask Unit ← async readLoop
  let writerTask : AsyncTask Unit ← async writeLoop
  let shutdown : Async Unit := do
    await readerTask
    await writerTask
    shutdownAction
  return { inbox, outbox, log, shutdown }

end Transport
end LeanWorker
