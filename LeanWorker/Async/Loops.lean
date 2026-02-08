module

public import LeanWorker.Transport.Types
public import LeanWorker.Transport.Codec
public import LeanWorker.JsonRpc.Encoding
public import Std.Sync.Channel
public import Std.Internal.Async.Basic

public section

namespace LeanWorker
namespace Async

open Lean
open JsonRpc
open Transport
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

partial def framedCodecTransport
    (rawTransport : Transport.Transport ByteArray ByteArray)
    (encodeFrame : ByteArray → ByteArray)
    (decodeFrame : ByteArray → Except Error (Array ByteArray × ByteArray))
    (codec : Codec Incoming Outgoing) :
    Async (Transport (Except Error Incoming) Outgoing) := do
  let inbox : Std.CloseableChannel (Except Error Incoming) ← Std.CloseableChannel.new
  let outbox : Std.CloseableChannel Outgoing ← Std.CloseableChannel.new
  let readerTask : AsyncTask Unit ← async do
    let rec readLoop (buffer : ByteArray) : Async Unit := do
      match ← await <| ← rawTransport.inbox.recv with
      | none =>
        closeOrLog rawTransport.log "typed inbox" inbox
      | some chunk =>
        let buffer := buffer ++ chunk
        match decodeFrame buffer with
        | .ok (payloads, rest) =>
          let mut continueLoop := true
          for payload in payloads do
            if continueLoop then
              match codec.decode payload with
              | .ok message =>
                let sent ← sendOrLog rawTransport.log "typed inbox" inbox (.ok message)
                if !sent then
                  continueLoop := false
              | .error err =>
                logError rawTransport.log s!"codec decode error: {errorToString err}"
                let sent ← sendOrLog rawTransport.log "typed inbox" inbox (.error err)
                if !sent then
                  continueLoop := false
          if continueLoop then
            readLoop rest
        | .error err =>
          logError rawTransport.log s!"framing decode error: {errorToString err}"
          let sent ← sendOrLog rawTransport.log "typed inbox" inbox (.error err)
          if sent then
            readLoop ByteArray.empty
    readLoop ByteArray.empty
  let writerTask : AsyncTask Unit ← async do
    let rec writeLoop : Async Unit := do
      match ← await <| ← outbox.recv with
      | none =>
        closeOrLog rawTransport.log "byte outbox" rawTransport.outbox
      | some message =>
        let payload := codec.encode message
        let bytes := encodeFrame payload
        let sent ← sendOrLog rawTransport.log "byte outbox" rawTransport.outbox bytes
        if sent then
          writeLoop
    writeLoop
  let shutdown : Async Unit := do
    rawTransport.shutdown
    await readerTask
    await writerTask
  return { inbox, outbox, log := rawTransport.log, shutdown }

end Async
end LeanWorker
