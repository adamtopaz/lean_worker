module

public import LeanWorker
public import LeanWorkerTest.FullServer.Types

public section

namespace LeanWorkerTest
namespace FullServer

open Lean
open LeanWorker
open LeanWorker.JsonRpc
open LeanWorker.Server

def requireParams {m : Type → Type} [Monad m] [MonadExcept Error m] (params? : Option α) : m α :=
  match params? with
  | some params => pure params
  | none => throw Error.invalidParams

def addOptionalField
    (fields : List (String × Json))
    (key : String)
    (value? : Option Json) : List (String × Json) :=
  match value? with
  | none => fields
  | some value => fields ++ [(key, value)]

def pingHandler : StatelessHandler FullContext Json.Structured String := fun _ =>
  return "pong"

def infoHandler : StatelessHandler FullContext Json.Structured Json := fun _ => do
  let ctx ← read
  return Json.mkObj
    [
      ("name", toJson ctx.name),
      ("version", toJson ctx.version),
      ("defaultDelayMs", toJson ctx.defaultDelayMs)
    ]

def echoHandler : StatelessHandler FullContext Json.Structured Json := fun params? =>
  match params? with
  | some params => return toJson params
  | none => return Json.null

def addHandler : StatelessHandler FullContext AddParams Int := fun params? => do
  let params ← requireParams params?
  return params.a + params.b

def sumHandler : StatelessHandler FullContext SumParams Int := fun params? => do
  let params ← requireParams params?
  return params.values.foldl (init := 0) (· + ·)

def sleepHandler : StatelessHandler FullContext SleepParams Json := fun params? => do
  let ctx ← read
  let ms : Nat := match params? with
    | some params => params.ms
    | none => ctx.defaultDelayMs
  let tag? := params?.bind fun params => params.tag?
  IO.sleep (UInt32.ofNat ms)
  let fields := addOptionalField [ ("ms", toJson ms) ] "tag" (tag?.map toJson)
  return Json.mkObj fields

def counterGetHandler : StatefulHandler FullContext FullState Json.Structured Nat := fun _ => do
  return (← get).counter

def counterAddHandler : StatefulHandler FullContext FullState CounterAddParams Nat := fun params? => do
  let params ← requireParams params?
  if params.delta < 0 then
    throw Error.invalidParams
  let delta := params.delta.toNat
  let state ← get
  let next := state.counter + delta
  set { state with counter := next }
  return next

def counterResetHandler : StatefulHandler FullContext FullState Json.Structured Nat := fun _ => do
  let state ← get
  set { state with counter := 0 }
  return state.counter

def kvGetHandler : StatefulHandler FullContext FullState KeyParam Json := fun params? => do
  let params ← requireParams params?
  let state ← get
  match state.kvs.get? params.key with
  | none =>
    return Json.mkObj [ ("found", Json.bool false) ]
  | some value =>
    return Json.mkObj [ ("found", Json.bool true), ("value", value) ]

def kvSetHandler : StatefulHandler FullContext FullState KeyValueParam Json := fun params? => do
  let params ← requireParams params?
  let state ← get
  let oldValue? := state.kvs.get? params.key
  let kvs := state.kvs.insert params.key params.value
  set { state with kvs := kvs }
  let fields := addOptionalField
    [ ("hadValue", Json.bool oldValue?.isSome) ]
    "oldValue" oldValue?
  return Json.mkObj fields

def kvDeleteHandler : StatefulHandler FullContext FullState KeyParam Bool := fun params? => do
  let params ← requireParams params?
  let state ← get
  let hadValue := (state.kvs.get? params.key).isSome
  let kvs := state.kvs.erase params.key
  set { state with kvs := kvs }
  return hadValue

def eventsListHandler : StatefulHandler FullContext FullState Json.Structured (Array String) := fun _ => do
  return (← get).events

def eventsClearHandler : StatefulHandler FullContext FullState Json.Structured Nat := fun _ => do
  let state ← get
  set { state with events := #[] }
  return state.events.size

def snapshotHandler : StatefulHandler FullContext FullState Json.Structured Json := fun _ => do
  let state ← get
  return Json.mkObj
    [
      ("counter", toJson state.counter),
      ("kvSize", toJson state.kvs.size),
      ("eventsSize", toJson state.events.size),
      ("inFlight", toJson state.inFlight)
    ]

def errorCustomHandler : StatelessHandler FullContext ErrorCustomParams Json := fun params? => do
  let params ← requireParams params?
  let ctx ← read
  if ctx.allowCustomErrors then
    throw { code := params.code, message := params.message, data? := params.data? }
  else
    throw <| Error.withMessage Error.invalidParams "custom errors disabled"

def errorInternalHandler : StatelessHandler FullContext Json.Structured Json := fun _ => do
  throw Error.internalError

def bumpNotification : StatefulNotification FullContext FullState NotifyBumpParams := fun params? => do
  let delta := match params? with
    | none => 1
    | some params => params.delta?.getD 1
  let state ← get
  set { state with counter := state.counter + delta }

def logNotification : StatefulNotification FullContext FullState NotifyLogParams := fun params? => do
  match params? with
  | none => return
  | some params =>
    let state ← get
    set { state with events := state.events.push params.message }

def sleepNotification : StatefulNotification FullContext FullState NotifySleepParams := fun params? => do
  let ctx ← read
  let ms : Nat := match params? with
    | none => ctx.defaultDelayMs
    | some params => params.ms
  let state ← get
  set { state with inFlight := state.inFlight + 1 }
  IO.sleep (UInt32.ofNat ms)
  let state ← get
  set { state with inFlight := state.inFlight - 1 }

def handlers : HandlerRegistry FullContext FullState :=
  HandlerRegistry.empty
    |>.addStateless "ping" pingHandler
    |>.addStateless "info" infoHandler
    |>.addStateless "echo" echoHandler
    |>.addStateless "add" addHandler
    |>.addStateless "sum" sumHandler
    |>.addStateless "sleep" sleepHandler
    |>.addStateful "counter.get" counterGetHandler
    |>.addStateful "counter.add" counterAddHandler
    |>.addStateful "counter.reset" counterResetHandler
    |>.addStateful "kv.get" kvGetHandler
    |>.addStateful "kv.set" kvSetHandler
    |>.addStateful "kv.delete" kvDeleteHandler
    |>.addStateful "events.list" eventsListHandler
    |>.addStateful "events.clear" eventsClearHandler
    |>.addStateful "state.snapshot" snapshotHandler
    |>.addStateless "error.custom" errorCustomHandler
    |>.addStateless "error.internal" errorInternalHandler

def notifications : NotificationRegistry FullContext FullState :=
  NotificationRegistry.empty
    |>.addStateful "notify.bump" bumpNotification
    |>.addStateful "notify.log" logNotification
    |>.addStateful "notify.sleep" sleepNotification

end FullServer
end LeanWorkerTest
