module

public import Lean
public import LeanWorker.JsonRpc.Structured

public section

namespace LeanWorker
namespace JsonRpc

open Lean

def jsonRpcVersion : String := "2.0"

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

end JsonRpc
end LeanWorker
