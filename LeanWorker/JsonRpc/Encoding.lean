module

public import LeanWorker.JsonRpc.Core

public section

namespace LeanWorker
namespace JsonRpc

open Lean

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

instance : ToJson RpcId where
  toJson
    | .str value => .str value
    | .num value => .num value
    | .null => .null

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
