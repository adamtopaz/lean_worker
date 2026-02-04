module

public import LeanWorker.Common
public import LeanWorker.Async.Loops
public import LeanWorker.Framing.Newline
public import Std.Data.HashMap
public import Std.Sync.Mutex

public section

namespace LeanWorker
namespace Client

open Lean
open JsonRpc
open Std.Internal.IO.Async

inductive Kind where
  | request
  | notification

structure Client where
  request : String → Option Json.Structured → EAsync Error Json
  notify : String → Option Json.Structured → EAsync Error Unit
  batch : Array (String × Option Json.Structured × Kind) →
    EAsync Error (Array <| Option <| Except Error Json)
  shutdown : Async Unit

def logLevelTag : Transport.LogLevel → String
  | .debug => "DEBUG"
  | .info => "INFO"
  | .warn => "WARN"
  | .error => "ERROR"

def stderrLogger (label : String) : IO (Transport.LogLevel → String → IO Unit) := do
  let stream ← IO.getStderr
  return fun level message => do
    stream.putStrLn s!"[{label} {logLevelTag level}] {message}"
    stream.flush

def defaultStderrLogger : IO (Transport.LogLevel → String → IO Unit) :=
  stderrLogger "CLIENT"

partial def streamReadLoop
    (readStream : IO.FS.Stream)
    (log : Transport.LogLevel → String → IO Unit)
    (inbox : Std.CloseableChannel ByteArray) : Async Unit := do
  let chunk ← readStream.read (USize.ofNat 4096)
  if chunk.size == 0 then
    LeanWorker.Async.closeOrLog log "byte inbox" inbox
  else
    let sent ← LeanWorker.Async.sendOrLog log "byte inbox" inbox chunk
    if sent then
      streamReadLoop readStream log inbox

partial def streamWriteLoop
    (writeStream : IO.FS.Stream)
    (log : Transport.LogLevel → String → IO Unit)
    (outbox : Std.CloseableChannel ByteArray) : Async Unit := do
  match ← await <| ← outbox.recv with
  | none => return
  | some bytes =>
    writeStream.write bytes
    writeStream.flush
    streamWriteLoop writeStream log outbox

def byteTransportFromStreams
    (readStream writeStream : IO.FS.Stream)
    (log : Transport.LogLevel → String → IO Unit)
    (shutdownAction : Async Unit := pure ()) : Async Transport.ByteTransport := do
  let inbox : Std.CloseableChannel ByteArray ← Std.CloseableChannel.new
  let outbox : Std.CloseableChannel ByteArray ← Std.CloseableChannel.new
  let readLoop : Async Unit := do
    try
      streamReadLoop readStream log inbox
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

def getClient
    (transport : Transport.Transport (Except Error Json) Json) : Async Client := do
  let nextId : Std.Mutex Nat ← Std.Mutex.new 0
  let pending : Std.Mutex (Std.HashMap RpcId (IO.Promise (Except Error Json))) ←
    Std.Mutex.new {}

  let logError (message : String) : Async Unit :=
    LeanWorker.Async.logError transport.log message

  let responseId : Response → RpcId
    | .result id _ => id
    | .error id _ => id

  let resolveResponse (response : Response) : Async Unit := do
    let id := responseId response
    let promise? ← pending.atomically do
      let entries ← get
      match entries.get? id with
      | some promise =>
        set (entries.erase id)
        return some promise
      | none =>
        return none
    match promise? with
    | none => logError s!"unknown response id: {Json.compress (toJson id)}"
    | some promise =>
      match response with
      | .result _ result => promise.resolve (.ok result)
      | .error _ err => promise.resolve (.error err)

  let rec resolveJson (json : Json) : Async Unit := do
    match json with
    | .arr items =>
      for item in items do
        resolveJson item
    | _ =>
      match fromJson? (α := Response) json with
      | .ok response => resolveResponse response
      | .error message =>
        logError s!"invalid response: {message}"

  let readerTask : AsyncTask Unit ← async do
    repeat
      match ← await <| ← transport.inbox.recv with
      | none => break
      | some (.error err) =>
        logError s!"transport error: {LeanWorker.Async.errorToString err}"
      | some (.ok json) =>
        resolveJson json
    let pendingEntries ← pending.atomically do
      let entries := (← get).toList
      set ({} : Std.HashMap RpcId (IO.Promise (Except Error Json)))
      return entries
    for (_, promise) in pendingEntries do
      promise.resolve (.error Error.internalError)

  let getNextId : BaseIO RpcId := nextId.atomically do
    let current ← get
    set (current + 1)
    return .num (current : JsonNumber)

  let sendJson (json : Json) : EAsync Error Unit := do
    match ← await <| ← transport.outbox.send json with
    | .ok _ => return
    | .error _ => throw Error.internalError

  let request : String → Option Json.Structured → EAsync Error Json := fun method params? => do
    let id ← getNextId
    let payload : Json := toJson ({ id := id, method := method, params? := params? } : Request)
    let promise : IO.Promise (Except Error Json) ← IO.Promise.new
    pending.atomically <| modify fun entries => entries.insert id promise
    match ← await <| ← transport.outbox.send payload with
    | .error _ =>
      pending.atomically <| modify fun entries => entries.erase id
      throw Error.internalError
    | .ok _ =>
      match ← await promise.result? with
      | some (.ok result) => return result
      | some (.error err) => throw err
      | none => throw Error.internalError

  let notify : String → Option Json.Structured → EAsync Error Unit := fun method params? => do
    let payload : Json := toJson ({ method := method, params? := params? } : Notification)
    sendJson payload

  let batch : Array (String × Option Json.Structured × Kind) →
      EAsync Error (Array <| Option <| Except Error Json) := fun items => do
    if items.isEmpty then
      throw Error.invalidRequest
    let mut promises : Array (Option (RpcId × IO.Promise (Except Error Json))) := #[]
    let mut payloads : Array Json := #[]
    for (method, params?, kind) in items do
      match kind with
      | .request =>
        let id ← getNextId
        let payload : Json := toJson ({ id := id, method := method, params? := params? } : Request)
        let promise : IO.Promise (Except Error Json) ← IO.Promise.new
        pending.atomically <| modify fun entries => entries.insert id promise
        payloads := payloads.push payload
        promises := promises.push (some (id, promise))
      | .notification =>
        let payload : Json := toJson ({ method := method, params? := params? } : Notification)
        payloads := payloads.push payload
        promises := promises.push none
    match ← await <| ← transport.outbox.send (.arr payloads) with
    | .error _ =>
      pending.atomically do
        for item in promises do
          match item with
          | some (id, _) =>
            modify fun entries => entries.erase id
          | none =>
            pure ()
      throw Error.internalError
    | .ok _ =>
      promises.mapM fun item => do
        match item with
        | none => return none
        | some (_, promise) =>
          match ← await promise.result? with
          | some (.ok result) => return some (.ok result)
          | some (.error err) => return some (.error err)
          | none => return some (.error Error.internalError)

  let shutdown : Async Unit := do
    LeanWorker.Async.closeOrLog transport.log "client outbox" transport.outbox
    transport.shutdown
    await readerTask

  return { request, notify, batch, shutdown }

structure SpawnConfig where
  cmd : String
  args : Array String := #[]
  cwd : Option System.FilePath := none
  env : Array (String × Option String) := #[]
  inheritEnv : Bool := true
  stderr : IO.Process.Stdio := .inherit

def spawnArgs (config : SpawnConfig) : IO.Process.SpawnArgs :=
  {
    cmd := config.cmd,
    args := config.args,
    cwd := config.cwd,
    env := config.env,
    inheritEnv := config.inheritEnv,
    stdin := .piped,
    stdout := .piped,
    stderr := config.stderr
  }

def spawnStdioTransport (config : SpawnConfig) : Async (Transport.Transport (Except Error Json) Json) := do
  let child ← IO.Process.spawn (spawnArgs config)
  let (stdinHandle, child) ← child.takeStdin
  let stdinStream := IO.FS.Stream.ofHandle stdinHandle
  let stdoutStream := IO.FS.Stream.ofHandle child.stdout
  let log ← defaultStderrLogger
  let shutdownAction : Async Unit := do
    let _ ← child.wait
    return
  let byteTransport ← byteTransportFromStreams stdoutStream stdinStream log shutdownAction
  LeanWorker.Async.framedTransport byteTransport Framing.newline

def spawnStdioClient (config : SpawnConfig) : Async Client := do
  let transport ← spawnStdioTransport config
  getClient transport

end Client
end LeanWorker
