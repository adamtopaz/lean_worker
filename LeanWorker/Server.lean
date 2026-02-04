module

public import LeanWorker.Common
public import Std.Data.HashMap
public import Std.Sync.Mutex

public section

namespace LeanWorker
namespace Server

open Lean
open JsonRpc
open Std.Internal.IO.Async

abbrev StatefulHandlerM (Context State : Type) := ReaderT Context (StateRefT State (EIO Error))
abbrev StatelessHandlerM (Context : Type) := ReaderT Context <| EIO Error
abbrev StatefulNotificationM (Context State : Type) := ReaderT Context (StateRefT State BaseIO)
abbrev StatelessNotificationM (Context : Type) := ReaderT Context <| BaseIO

abbrev StatefulHandler (Context State α β : Type) :=
  Option α → StatefulHandlerM Context State β

abbrev StatelessHandler (Context α β : Type) :=
  Option α → StatelessHandlerM Context β

abbrev StatefulNotification (Context State α : Type) :=
  Option α → StatefulNotificationM Context State Unit

abbrev StatelessNotification (Context α : Type) :=
  Option α → StatelessNotificationM Context Unit

inductive Handler (Context State : Type) where
  | stateful (run : Option Json.Structured → StatefulHandlerM Context State Json)
  | stateless (run : Option Json.Structured → StatelessHandlerM Context Json)

inductive Notification (Context State : Type) where
  | stateful (run : Option Json.Structured → StatefulNotificationM Context State Unit)
  | stateless (run : Option Json.Structured → StatelessNotificationM Context Unit)

def Handler.ofStateful [FromStructured α] [ToJson β]
    (handler : StatefulHandler Context State α β) : Handler Context State :=
  .stateful fun params? => do
    let params? ←
      MonadExcept.ofExcept (m := StatefulHandlerM Context State) <|
        decodeParams? (α := α) params?
    let result ← handler params?
    return toJson result

def Handler.ofStateless [FromStructured α] [ToJson β]
    (handler : StatelessHandler Context α β) : Handler Context State :=
  .stateless fun params? => do
    let params? ←
      MonadExcept.ofExcept (m := StatelessHandlerM Context) <|
        decodeParams? (α := α) params?
    let result ← handler params?
    return toJson result

def Notification.ofStateful [FromStructured α]
    (notification : StatefulNotification Context State α) : Notification Context State :=
  .stateful fun params? => do
    match decodeParams? (α := α) params? with
    | .ok value => notification value
    | .error _ => return

def Notification.ofStateless [FromStructured α]
    (notification : StatelessNotification Context α) : Notification Context State :=
  .stateless fun params? => do
    match decodeParams? (α := α) params? with
    | .ok value => notification value
    | .error _ => return

structure HandlerRegistry (Context State : Type) where
  entries : Std.HashMap String (Handler Context State)

def HandlerRegistry.empty : HandlerRegistry Context State where
  entries := {}

def HandlerRegistry.add
    (registry : HandlerRegistry Context State)
    (method : String)
    (handler : Handler Context State) : HandlerRegistry Context State :=
  { entries := registry.entries.insert method handler }

def HandlerRegistry.addStateful
    [FromStructured α] [ToJson β]
    (registry : HandlerRegistry Context State)
    (method : String)
    (handler : StatefulHandler Context State α β) : HandlerRegistry Context State :=
  registry.add method (Handler.ofStateful handler)

def HandlerRegistry.addStateless
    [FromStructured α] [ToJson β]
    (registry : HandlerRegistry Context State)
    (method : String)
    (handler : StatelessHandler Context α β) : HandlerRegistry Context State :=
  registry.add method (Handler.ofStateless handler)

structure NotificationRegistry (Context State : Type) where
  entries : Std.HashMap String (Notification Context State)

def NotificationRegistry.empty : NotificationRegistry Context State where
  entries := {}

def NotificationRegistry.add
    (registry : NotificationRegistry Context State)
    (method : String)
    (notification : Notification Context State) : NotificationRegistry Context State :=
  { entries := registry.entries.insert method notification }

def NotificationRegistry.addStateful
    [FromStructured α]
    (registry : NotificationRegistry Context State)
    (method : String)
    (notification : StatefulNotification Context State α) : NotificationRegistry Context State :=
  registry.add method (Notification.ofStateful notification)

def NotificationRegistry.addStateless
    [FromStructured α]
    (registry : NotificationRegistry Context State)
    (method : String)
    (notification : StatelessNotification Context α) : NotificationRegistry Context State :=
  registry.add method (Notification.ofStateless notification)

structure Server (Context State : Type) where
  handlers : HandlerRegistry Context State
  notifications : NotificationRegistry Context State
  transport : Transport.Transport (Except Error Json) Json
  maxTasks : Option Nat := none

def run (server : Server Context State) (ctx : Context) (initState : State) : Async Unit := do
  let state ← Std.Mutex.new initState
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
