module

public import LeanWorker.Server.Types
public import LeanWorker.Async.Loops
public import LeanWorker.JsonRpc.Parse
public import LeanWorker.JsonRpc.Encoding
public import Std.Sync.Mutex
public import Std.Internal.Async.Basic

public section

namespace LeanWorker
namespace Server

open Lean
open JsonRpc
open Std.Internal.IO.Async

def run (server : Server Context State) (ctx : Context) (state : Std.Mutex State) : Async Unit := do
  let remainingTasks ← mainLoop state
  for task in remainingTasks do
    await task
where
  filterTasks (tasks : Array (AsyncTask Unit)) : BaseIO (Array (AsyncTask Unit)) :=
    tasks.filterM fun task => return (← task.getState) != .finished

  sendJson (json : Json) : Async Unit := do
    let _ ← LeanWorker.Async.sendOrLog server.transport.log "server outbox" server.transport.outbox json
    return

  runHandler
      (state : Std.Mutex State)
      (handler : Handler Context State)
      (params? : Option Json.Structured) : Async (Except Error Json) := do
    match handler with
    | .stateful method =>
      state.atomically do
        let go := method params? ctx |>.run (← get)
        match ← go.toBaseIO with
        | .ok (result, next) =>
          set next
          return .ok result
        | .error err =>
          return .error err
    | .stateless method =>
      match ← (method params? ctx).toBaseIO with
      | .ok result => return .ok result
      | .error err => return .error err

  runNotification
      (state : Std.Mutex State)
      (notification : Notification Context State)
      (params? : Option Json.Structured) : Async Unit := do
    match notification with
    | .stateful method =>
      state.atomically do
        let ((), next) ← method params? ctx |>.run (← get)
        set next
    | .stateless method =>
      method params? ctx

  handleRequest (state : Std.Mutex State) (request : Request) : Async Response := do
    match server.handlers.entries.get? request.method with
    | none =>
      return responseError request.id Error.methodNotFound
    | some handler =>
      match ← runHandler state handler request.params? with
      | .ok result =>
        return responseResult request.id result
      | .error err =>
        return responseError request.id err

  handleMessage (state : Std.Mutex State) (message : JsonRpc.Message) : Async (Option Response) := do
    match message with
    | .request request =>
      some <$> handleRequest state request
    | .notification notification =>
      match server.notifications.entries.get? notification.method with
      | none => return none
      | some handler =>
        runNotification state handler notification.params?
        return none
    | .response _ =>
      return some invalidRequestResponse

  processSingle (state : Std.Mutex State) (json : Json) : Async (Option Response) :=
    match parseMessage json with
    | .ok message => handleMessage state message
    | .error err =>
      return some <| responseError .null err

  handleBatch (state : Std.Mutex State) (items : Array Json) : Async Unit := do
    if items.isEmpty then
      sendJson (toJson invalidRequestResponse)
    else
      let mut tasks : Array (AsyncTask (Option Response)) := #[]
      for item in items do
        tasks := tasks.push <| ← async <| processSingle state item
      let responses : Array Response ← tasks.filterMapM fun task => await task
      unless responses.isEmpty do
        sendJson (toJson responses)

  handleInput (state : Std.Mutex State) (input : Except Error Json) : Async Unit := do
    match input with
    | .error err =>
      sendJson (toJson <| responseError .null err)
    | .ok json =>
      match json with
      | .arr items => handleBatch state items
      | _ =>
        match ← processSingle state json with
        | some response => sendJson (toJson response)
        | none => return

  mainLoop (state : Std.Mutex State) : Async (Array (AsyncTask Unit)) := do
    let mut tasks : Array (AsyncTask Unit) := #[]
    repeat
      if let some max := server.maxTasks then
        while tasks.size > max do
          tasks ← filterTasks tasks
      match ← await <| ← server.transport.inbox.recv with
      | none => break
      | some input =>
        let task ← async <| handleInput state input
        tasks := tasks.push task
      tasks ← filterTasks tasks
    return tasks

end Server
end LeanWorker
