# Server Runtime

The server runtime is implemented in `LeanWorker/Server.lean`.

## Core Types

```lean
structure Server (Context State : Type) where
  handlers : HandlerRegistry Context State
  notifications : NotificationRegistry Context State
  transport : Transport.Transport (Except Error Json) Json
  maxTasks : Option Nat := none
```

Handlers and notifications are stored in registries keyed by method name.

## Handler Types

- `StatefulHandlerM` / `StatelessHandlerM`: request handlers
- `StatefulNotificationM` / `StatelessNotificationM`: notification handlers
- `Handler` / `Notification`: wrapped variants

## Registries

`HandlerRegistry` and `NotificationRegistry` provide helpers:

```lean
HandlerRegistry.empty
  |>.addStateless "ping" (fun _ => return "pong")
  |>.addStateful "counter.add" handler
```

These helpers decode params via `FromStructured` and map decode failures to `Error.invalidParams`.

## Running a Server

`Server.run` now takes a shared mutex:

```lean
def run : Async Unit := do
  let state ← Std.Mutex.new 0
  Server.run server () state
```

## Example: Stateful Handler and Notification

```lean
open LeanWorker
open LeanWorker.JsonRpc
open LeanWorker.Server

structure AddParams where
  delta : Int

instance : FromStructured AddParams where
  fromStructured? params := do
    let kvs ← JsonRpc.expectObj params
    let deltaJson ← JsonRpc.requireField kvs "delta"
    let delta ← deltaJson.getInt?
    return { delta }

def addHandler : StatefulHandler Unit Nat AddParams Nat := fun params? => do
  let params ← match params? with
    | some p => pure p
    | none => throw Error.invalidParams
  if params.delta < 0 then
    throw Error.invalidParams
  let current ← get
  let next := current + params.delta.toNat
  set next
  return next

def bumpNotification : StatefulNotification Unit Nat Json.Structured := fun _ => do
  let current ← get
  set (current + 1)

def handlers : HandlerRegistry Unit Nat :=
  HandlerRegistry.empty
    |>.addStateful "counter.add" addHandler

def notifications : NotificationRegistry Unit Nat :=
  NotificationRegistry.empty
    |>.addStateful "notify.bump" bumpNotification
```

This allows you to share state across connections by reusing the same mutex.

## Notifications

Notifications never produce responses. Invalid params for notifications are ignored rather than returning an error.

## Concurrency

- Each incoming message is handled in a task.
- `maxTasks` limits concurrent work and prevents unbounded task creation.
- State mutations are serialized by `Std.Mutex State`.
