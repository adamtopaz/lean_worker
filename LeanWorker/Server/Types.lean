module

public import LeanWorker.JsonRpc.Parse
public import LeanWorker.Transport.Types
public import Std.Data.HashMap

public section

namespace LeanWorker
namespace Server

open Lean
open JsonRpc

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

end Server
end LeanWorker
