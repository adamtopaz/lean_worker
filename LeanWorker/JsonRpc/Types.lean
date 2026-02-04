module

public import Lean
public import LeanWorker.JsonRpc.Structured

public section

namespace LeanWorker
namespace JsonRpc

open Lean

def jsonRpcVersion : String := "2.0"

def requireField
    (kvs : Std.TreeMap.Raw String Json compare)
    (key : String) : Except String Json :=
  match kvs.get? key with
  | some value => return value
  | none => throw s!"property not found: {key}"

structure Error where
  code : Int
  message : String
  data? : Option Json := none

def Error.parseError : Error := { code := -32700, message := "Parse error" }
def Error.invalidRequest : Error := { code := -32600, message := "Invalid Request" }
def Error.methodNotFound : Error := { code := -32601, message := "Method not found" }
def Error.invalidParams : Error := { code := -32602, message := "Invalid params" }
def Error.internalError : Error := { code := -32603, message := "Internal error" }

def Error.withMessage (err : Error) (message : String) : Error :=
  { err with message }

def Error.withData (err : Error) (data : Json) : Error :=
  { err with data? := some data }

instance : ToJson Error where
  toJson err :=
    let fields :=
      [
        ("code", toJson err.code),
        ("message", toJson err.message)
      ]
    let fields :=
      match err.data? with
      | some data => fields ++ [("data", data)]
      | none => fields
    Json.mkObj fields

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

inductive RpcId where
  | str (value : String)
  | num (value : JsonNumber)
  | null
deriving BEq, Hashable

instance : Ord RpcId where
  compare
    | .str a, .str b => compare a b
    | .str _, .num _ => .gt
    | .str _, .null => .gt
    | .num _, .str _ => .lt
    | .num a, .num b => compare a b
    | .num _, .null => .gt
    | .null, .str _ => .lt
    | .null, .num _ => .lt
    | .null, .null => .eq

def jsonNumberIsInteger (number : JsonNumber) : Bool :=
  if number.exponent == 0 then
    true
  else
    let divisor : Int := Int.ofNat (10 ^ number.exponent)
    number.mantissa % divisor == 0

instance : ToJson RpcId where
  toJson
    | .str value => .str value
    | .num value => .num value
    | .null => .null

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

structure Request where
  id : RpcId
  method : String
  params? : Option Json.Structured := none

structure Notification where
  method : String
  params? : Option Json.Structured := none

inductive Response where
  | result (id : RpcId) (result : Json)
  | error (id : RpcId) (error : Error)

inductive Message where
  | request (value : Request)
  | notification (value : Notification)
  | response (value : Response)

inductive Batch (α : Type) where
  | single : α → Batch α
  | batch : Array α → Batch α

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

instance : ToJson Request where
  toJson req :=
    let fields :=
      [
        ("jsonrpc", Json.str jsonRpcVersion),
        ("id", toJson req.id),
        ("method", Json.str req.method)
      ]
    let fields :=
      match req.params? with
      | some params => fields ++ [("params", toJson params)]
      | none => fields
    Json.mkObj fields

instance : ToJson Notification where
  toJson note :=
    let fields :=
      [
        ("jsonrpc", Json.str jsonRpcVersion),
        ("method", Json.str note.method)
      ]
    let fields :=
      match note.params? with
      | some params => fields ++ [("params", toJson params)]
      | none => fields
    Json.mkObj fields

instance : ToJson Response where
  toJson
    | .result id result =>
      Json.mkObj
        [
          ("jsonrpc", Json.str jsonRpcVersion),
          ("id", toJson id),
          ("result", result)
        ]
    | .error id err =>
      Json.mkObj
        [
          ("jsonrpc", Json.str jsonRpcVersion),
          ("id", toJson id),
          ("error", toJson err)
        ]

instance : ToJson Message where
  toJson
    | .request value => toJson value
    | .notification value => toJson value
    | .response value => toJson value

instance [ToJson α] : ToJson (Batch α) where
  toJson
    | .single item => toJson item
    | .batch items => toJson items

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

def responseResult (id : RpcId) (result : Json) : Response :=
  .result id result

def responseError (id : RpcId) (error : Error) : Response :=
  .error id error

def parseErrorResponse : Response :=
  responseError .null Error.parseError

def invalidRequestResponse : Response :=
  responseError .null Error.invalidRequest

end JsonRpc
end LeanWorker
