module

public import LeanWorker
public import Std.Data.HashMap

public section

namespace LeanWorkerTest
namespace FullServer

open Lean
open LeanWorker
open LeanWorker.JsonRpc

structure FullContext where
  name : String
  version : String
  defaultDelayMs : Nat
  allowCustomErrors : Bool

structure FullState where
  counter : Nat
  kvs : Std.HashMap String Json
  events : Array String
  inFlight : Nat

def emptyState : FullState :=
  {
    counter := 0,
    kvs := {},
    events := #[],
    inFlight := 0
  }

def expectObj
    (params : Json.Structured)
    : Except String (Std.TreeMap.Raw String Json compare) :=
  match params with
  | .obj kvs => return kvs
  | _ => throw "object params expected"

def requireObjField
    (kvs : Std.TreeMap.Raw String Json compare)
    (key : String) : Except String Json :=
  JsonRpc.requireField kvs key

def jsonToInt (json : Json) : Except String Int :=
  json.getInt?

def jsonToNat (json : Json) : Except String Nat := do
  let value ← json.getInt?
  if value < 0 then
    throw "expected non-negative integer"
  else
    return value.toNat

def jsonToString (json : Json) : Except String String :=
  json.getStr?

structure AddParams where
  a : Int
  b : Int

instance : FromStructured AddParams where
  fromStructured? params := do
    let kvs ← expectObj params
    let aJson ← requireObjField kvs "a"
    let bJson ← requireObjField kvs "b"
    let a ← jsonToInt aJson
    let b ← jsonToInt bJson
    return { a, b }

structure SumParams where
  values : Array Int

instance : FromStructured SumParams where
  fromStructured?
    | .arr elems => do
      let values ← elems.mapM jsonToInt
      return { values }
    | _ => throw "array params expected"

structure SleepParams where
  ms : Nat
  tag? : Option String

instance : FromStructured SleepParams where
  fromStructured? params := do
    let kvs ← expectObj params
    let msJson ← requireObjField kvs "ms"
    let ms ← jsonToNat msJson
    let tag? := kvs.get? "tag"
    match tag? with
    | none => return { ms, tag? := none }
    | some tagJson =>
      let tag ← jsonToString tagJson
      return { ms, tag? := some tag }

structure CounterAddParams where
  delta : Int

instance : FromStructured CounterAddParams where
  fromStructured? params := do
    let kvs ← expectObj params
    let deltaJson ← requireObjField kvs "delta"
    let delta ← jsonToInt deltaJson
    return { delta }

structure KeyParam where
  key : String

instance : FromStructured KeyParam where
  fromStructured? params := do
    let kvs ← expectObj params
    let keyJson ← requireObjField kvs "key"
    let key ← jsonToString keyJson
    return { key }

structure KeyValueParam where
  key : String
  value : Json

instance : FromStructured KeyValueParam where
  fromStructured? params := do
    let kvs ← expectObj params
    let keyJson ← requireObjField kvs "key"
    let value ← requireObjField kvs "value"
    let key ← jsonToString keyJson
    return { key, value }

structure ErrorCustomParams where
  code : Int
  message : String
  data? : Option Json

instance : FromStructured ErrorCustomParams where
  fromStructured? params := do
    let kvs ← expectObj params
    let codeJson ← requireObjField kvs "code"
    let messageJson ← requireObjField kvs "message"
    let code ← jsonToInt codeJson
    let message ← jsonToString messageJson
    let data? := kvs.get? "data"
    return { code, message, data? }

structure NotifyBumpParams where
  delta? : Option Nat

instance : FromStructured NotifyBumpParams where
  fromStructured? params := do
    let kvs ← expectObj params
    let delta? := kvs.get? "delta"
    match delta? with
    | none => return { delta? := none }
    | some deltaJson =>
      let delta ← jsonToNat deltaJson
      return { delta? := some delta }

structure NotifyLogParams where
  message : String

instance : FromStructured NotifyLogParams where
  fromStructured? params := do
    let kvs ← expectObj params
    let messageJson ← requireObjField kvs "message"
    let message ← jsonToString messageJson
    return { message }

structure NotifySleepParams where
  ms : Nat

instance : FromStructured NotifySleepParams where
  fromStructured? params := do
    let kvs ← expectObj params
    let msJson ← requireObjField kvs "ms"
    let ms ← jsonToNat msJson
    return { ms }

end FullServer
end LeanWorkerTest
