module

public import LeanWorker.JsonRpc.Core

public section

namespace LeanWorker
namespace JsonRpc

open Lean

def requireField
    (kvs : Std.TreeMap.Raw String Json compare)
    (key : String) : Except String Json :=
  match kvs.get? key with
  | some value => return value
  | none => throw s!"property not found: {key}"

instance : FromJson Error where
  fromJson? json := do
    match json with
    | .obj kvs =>
      let codeJson ← requireField kvs "code"
      let messageJson ← requireField kvs "message"
      let code ← codeJson.getInt?
      let message ← messageJson.getStr?
      let data? := kvs.get? "data"
      return { code, message, data? }
    | _ => throw "object expected"

def jsonNumberIsInteger (number : JsonNumber) : Bool :=
  if number.exponent == 0 then
    true
  else
    let divisor : Int := Int.ofNat (10 ^ number.exponent)
    number.mantissa % divisor == 0

instance : FromJson RpcId where
  fromJson?
    | .str value => return .str value
    | .num value =>
      if jsonNumberIsInteger value then
        return .num value
      else
        throw "id must not contain fractional parts"
    | .null => return .null
    | _ => throw "id must be string, number, or null"

def expectJsonRpcVersion
    (kvs : Std.TreeMap.Raw String Json compare) : Except String Unit := do
  let versionJson ← requireField kvs "jsonrpc"
  let version ← versionJson.getStr?
  if version == jsonRpcVersion then
    return ()
  else
    throw "jsonrpc must be 2.0"

def parseMethodName (json : Json) : Except String String := do
  let method ← json.getStr?
  if method.startsWith "rpc." then
    throw "method name is reserved"
  else
    return method

def parseParams?
    (kvs : Std.TreeMap.Raw String Json compare) : Except String (Option Json.Structured) :=
  match kvs.get? "params" with
  | none => return none
  | some json =>
    match json with
    | .arr elems => return some <| .arr elems
    | .obj obj => return some <| .obj obj
    | _ => throw "params must be array or object"

def parseRequest
    (kvs : Std.TreeMap.Raw String Json compare) : Except String Request := do
  expectJsonRpcVersion kvs
  let methodJson ← requireField kvs "method"
  let method ← parseMethodName methodJson
  let params? ← parseParams? kvs
  let idJson ← requireField kvs "id"
  let id ← fromJson? (α := RpcId) idJson
  return { id, method, params? }

def parseNotification
    (kvs : Std.TreeMap.Raw String Json compare) : Except String Notification := do
  expectJsonRpcVersion kvs
  let methodJson ← requireField kvs "method"
  let method ← parseMethodName methodJson
  let params? ← parseParams? kvs
  return { method, params? }

def parseResponse
    (kvs : Std.TreeMap.Raw String Json compare) : Except String Response := do
  expectJsonRpcVersion kvs
  let idJson ← requireField kvs "id"
  let id ← fromJson? (α := RpcId) idJson
  let result? := kvs.get? "result"
  let error? := kvs.get? "error"
  match result?, error? with
  | some result, none => return .result id result
  | none, some errorJson =>
    let err ← fromJson? (α := Error) errorJson
    return .error id err
  | none, none => throw "response must contain result or error"
  | some _, some _ => throw "response must not contain result and error"

def parseMessageObject
    (kvs : Std.TreeMap.Raw String Json compare) : Except String Message := do
  let method? := kvs.get? "method"
  let result? := kvs.get? "result"
  let error? := kvs.get? "error"
  match method? with
  | some _ =>
    if result?.isSome || error?.isSome then
      throw "request must not contain result or error"
    match kvs.get? "id" with
    | some _ => return .request (← parseRequest kvs)
    | none => return .notification (← parseNotification kvs)
  | none =>
    if result?.isSome || error?.isSome then
      return .response (← parseResponse kvs)
    else
      throw "message must contain method or result/error"

instance : FromJson Message where
  fromJson? json :=
    match json with
    | .obj kvs => parseMessageObject kvs
    | _ => throw "object expected"

instance : FromJson Request where
  fromJson? json := do
    match ← fromJson? (α := Message) json with
    | .request value => return value
    | _ => throw "request object expected"

instance : FromJson Notification where
  fromJson? json := do
    match ← fromJson? (α := Message) json with
    | .notification value => return value
    | _ => throw "notification object expected"

instance : FromJson Response where
  fromJson? json := do
    match ← fromJson? (α := Message) json with
    | .response value => return value
    | _ => throw "response object expected"

instance [FromJson α] : FromJson (Batch α) where
  fromJson?
    | .arr items => do
      if items.isEmpty then
        throw "batch must not be empty"
      let items : Array α ← items.mapM fromJson?
      return .batch items
    | item => return .single (← fromJson? (α := α) item)

def decodeParams
    [FromStructured α]
    (params : Json.Structured) : Except Error α :=
  match FromStructured.fromStructured? (α := α) params with
  | .ok value => return value
  | .error message =>
    throw <| Error.withData Error.invalidParams (Json.str message)

def decodeParams?
    [FromStructured α]
    (params? : Option Json.Structured) : Except Error (Option α) :=
  match params? with
  | none => return none
  | some params => some <$> decodeParams (α := α) params

def parseJson (input : String) : Except Error Json :=
  match Json.parse input with
  | .ok json => return json
  | .error message =>
    throw <| Error.withData Error.parseError (Json.str message)

def parseMessage (json : Json) : Except Error Message :=
  match fromJson? (α := Message) json with
  | .ok message => return message
  | .error message =>
    throw <| Error.withData Error.invalidRequest (Json.str message)

def parseBatch (json : Json) : Except Error (Batch Message) :=
  match fromJson? (α := Batch Message) json with
  | .ok batch => return batch
  | .error message =>
    throw <| Error.withData Error.invalidRequest (Json.str message)

end JsonRpc
end LeanWorker
