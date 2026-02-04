module

public import LeanWorker.Transport.Types
public import LeanWorker.Framing.Types
public import LeanWorker.JsonRpc.Types
public import Std.Sync.Channel
public import Std.Internal.Async.Basic

public section

namespace LeanWorker
namespace Async

open Lean
open JsonRpc
open Transport
open Framing
open Std.Internal.IO.Async

def errorToString (err : Error) : String :=
  Json.compress (toJson err)

def logAsync
    (log : LogLevel → String → IO Unit)
    (level : LogLevel)
    (message : String) : Async Unit :=
  EAsync.lift (log level message)

def logError
    (log : LogLevel → String → IO Unit)
    (message : String) : Async Unit :=
  logAsync log .error message

def sendOrLog
    (log : LogLevel → String → IO Unit)
    (label : String)
    (channel : Std.CloseableChannel α)
    (value : α) : Async Bool := do
  match ← await <| ← channel.send value with
  | .ok _ => return true
  | .error err =>
    logError log s!"{label} send failed: {err}"
    return false

def closeOrLog
    (log : LogLevel → String → IO Unit)
    (label : String)
    (channel : Std.CloseableChannel α) : Async Unit := do
  match ← channel.close.toBaseIO with
  | .ok _ => return
  | .error err =>
    logError log s!"{label} close failed: {err}"

partial def readLoop
    (byteTransport : ByteTransport)
    (framing : Framing)
    (inbox : Std.CloseableChannel (Except Error Json)) : Async Unit := do
  let rec loop (buffer : ByteArray) : Async Unit := do
    match ← await <| ← byteTransport.inbox.recv with
    | none =>
      closeOrLog byteTransport.log "json inbox" inbox
    | some chunk =>
      let buffer := buffer ++ chunk
      match framing.decode buffer with
      | .ok (messages, rest) =>
        let mut continueLoop := true
        for msg in messages do
          if continueLoop then
            let sent ← sendOrLog byteTransport.log "json inbox" inbox (.ok msg)
            if !sent then
              continueLoop := false
        if continueLoop then
          loop rest
      | .error err =>
        logError byteTransport.log s!"framing decode error: {errorToString err}"
        let sent ← sendOrLog byteTransport.log "json inbox" inbox (.error err)
        if sent then
          loop ByteArray.empty
  loop ByteArray.empty

partial def writeLoop
    (byteTransport : ByteTransport)
    (framing : Framing)
    (outbox : Std.CloseableChannel Json) : Async Unit := do
  let rec loop : Async Unit := do
    match ← await <| ← outbox.recv with
    | none =>
      closeOrLog byteTransport.log "byte outbox" byteTransport.outbox
    | some msg =>
      let bytes := framing.encode msg
      let sent ← sendOrLog byteTransport.log "byte outbox" byteTransport.outbox bytes
      if sent then
        loop
  loop

def framedTransport
    (byteTransport : ByteTransport)
    (framing : Framing) : Async (Transport (Except Error Json) Json) := do
  let inbox : Std.CloseableChannel (Except Error Json) ← Std.CloseableChannel.new
  let outbox : Std.CloseableChannel Json ← Std.CloseableChannel.new
  let readerTask : AsyncTask Unit ← async <| readLoop byteTransport framing inbox
  let writerTask : AsyncTask Unit ← async <| writeLoop byteTransport framing outbox
  let shutdown : Async Unit := do
    byteTransport.shutdown
    await readerTask
    await writerTask
  return { inbox, outbox, log := byteTransport.log, shutdown }

end Async
end LeanWorker
