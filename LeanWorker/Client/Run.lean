module

public import LeanWorker.Client.Types
public import LeanWorker.Transport.Logging
public import LeanWorker.JsonRpc.Parse
public import LeanWorker.JsonRpc.Encoding
public import LeanWorker.Transport.Spawn
public import Std.Data.HashMap
public import Std.Sync.Mutex

public section

namespace LeanWorker
namespace Client

open Lean
open JsonRpc
open Std.Internal.IO.Async

private partial def awaitTaskWithTimeout
    (task : AsyncTask Unit)
    (remainingPolls : Nat)
    (pollMs : UInt32 := 20) : Async (Except String Bool) := do
  match remainingPolls with
  | 0 =>
    return .ok false
  | remainingPolls + 1 =>
    if (← task.getState) == .finished then
      try
        await task
        return .ok true
      catch err =>
        return .error s!"task failed while awaiting completion: {err}"
    else
      IO.sleep pollMs
      awaitTaskWithTimeout task remainingPolls pollMs

def getClient
    (transport : LeanWorker.Transport.ClientTransport) : Async Client := do
  let nextId : Std.Mutex Nat ← Std.Mutex.new 0
  let pending : Std.Mutex (Std.HashMap RpcId (IO.Promise (Except Error Json))) ←
    Std.Mutex.new {}

  let logError (message : String) : Async Unit :=
    LeanWorker.Transport.logError transport.log message

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
        logError s!"transport error: {LeanWorker.Transport.errorToString err}"
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

  let shutdown : Async (Except String Unit) := do
    let transportResult ←
      try
        transport.shutdown
      catch err =>
        pure (.error s!"transport shutdown threw exception: {err}")
    let readerError? ←
      match ← awaitTaskWithTimeout readerTask 50 with
      | .ok true =>
        pure none
      | .ok false =>
        let message := "client reader task did not finish during shutdown"
        logError message
        pure (some message)
      | .error err =>
        let message := s!"client reader task failed during shutdown: {err}"
        logError message
        pure (some message)
    let mut errors : Array String := #[]
    match transportResult with
    | .ok _ =>
      pure ()
    | .error message =>
      errors := errors.push message
    match readerError? with
    | some message =>
      errors := errors.push message
    | none =>
      pure ()
    if errors.isEmpty then
      return .ok ()
    else
      return .error (String.intercalate "; " errors.toList)

  return { request, notify, batch, shutdown }

def spawnStdioClient
    (config : Transport.SpawnConfig)
    (frameSpec : Transport.FrameSpec := .newline) : Async Client := do
  let log ← Transport.stderrLogger "CLIENT"
  let transport ← Transport.spawnStdioClientTransport config frameSpec (log := log)
  getClient transport

end Client
end LeanWorker
