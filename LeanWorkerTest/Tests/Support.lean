module

public import LeanWorker

public section

namespace LeanWorkerTest

open Lean
open LeanWorker
open LeanWorker.JsonRpc

def assert (cond : Bool) (message : String) : IO Unit :=
  if cond then
    return
  else
    throw <| IO.userError message

def runTest (name : String) (action : IO Unit) : IO Unit := do
  let err ← IO.getStderr
  err.putStrLn s!"[TEST] {name}"
  err.flush
  action
  err.putStrLn s!"[OK] {name}"
  err.flush

def expectErrorResponse (json : Json) (code : Int) (id : RpcId) : IO Unit := do
  match fromJson? (α := Response) json with
  | .ok (.error respId err) =>
    assert (respId == id) s!"unexpected id: {Json.compress (toJson respId)}"
    assert (err.code == code) s!"unexpected error code: {err.code}"
  | .ok _ =>
    throw <| IO.userError "expected error response"
  | .error msg =>
    throw <| IO.userError s!"failed to decode response: {msg}"

def expectResultResponse (json : Json) (id : RpcId) : IO Json := do
  match fromJson? (α := Response) json with
  | .ok (.result respId result) =>
    assert (respId == id) s!"unexpected id: {Json.compress (toJson respId)}"
    return result
  | .ok _ =>
    throw <| IO.userError "expected result response"
  | .error msg =>
    throw <| IO.userError s!"failed to decode response: {msg}"

def decodeResponses (json : Json) : IO (Array Response) := do
  match json with
  | .arr items =>
    items.mapM fun item =>
      match fromJson? (α := Response) item with
      | .ok response => return response
      | .error msg => throw <| IO.userError s!"invalid response in batch: {msg}"
  | _ => throw <| IO.userError "expected batch response array"

def hasError (responses : Array Response) (code : Int) (id : RpcId) : Bool :=
  responses.any fun response =>
    match response with
    | .error respId err => respId == id && err.code == code
    | _ => false

def findResult (responses : Array Response) (id : RpcId) : Option Json :=
  responses.findSome? fun response =>
    match response with
    | .result respId result => if respId == id then some result else none
    | _ => none

def objParams (fields : List (String × Json)) : Json.Structured :=
  match Json.mkObj fields with
  | .obj kvs => .obj kvs
  | _ => .obj {}

def expectObj (json : Json) : IO (Std.TreeMap.Raw String Json compare) := do
  match json with
  | .obj kvs => return kvs
  | _ => throw <| IO.userError "expected object"

def expectObjField (json : Json) (key : String) : IO Json := do
  let kvs ← expectObj json
  match kvs.get? key with
  | some value => return value
  | none => throw <| IO.userError s!"missing field: {key}"

def jsonToNat (json : Json) : IO Nat := do
  match json.getInt? with
  | .ok value =>
    if value < 0 then
      throw <| IO.userError "expected non-negative integer"
    else
      return value.toNat
  | .error msg => throw <| IO.userError msg

def jsonToString (json : Json) : IO String := do
  match json.getStr? with
  | .ok value => return value
  | .error msg => throw <| IO.userError msg

def jsonToBool (json : Json) : IO Bool := do
  match json with
  | .bool value => return value
  | _ => throw <| IO.userError "expected boolean"

def sleepMs (ms : Nat) : IO Unit :=
  IO.sleep (UInt32.ofNat ms)

def monoMsNow : IO Nat := do
  IO.monoMsNow

def sleepParams (ms : Nat) (tag? : Option String := none) : Json.Structured :=
  let fields := match tag? with
    | none => [("ms", toJson ms)]
    | some tag => [("ms", toJson ms), ("tag", Json.str tag)]
  objParams fields

end LeanWorkerTest
