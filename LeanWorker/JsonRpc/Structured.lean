module

public import Lean

public section

namespace LeanWorker
namespace JsonRpc

open Lean

class FromStructured (α : Type) where
  fromStructured? : Json.Structured → Except String α

export FromStructured (fromStructured?)

class ToStructured (α : Type) where
  toStructured : α → Json.Structured

export ToStructured (toStructured)

def structuredFromJson? (json : Json) : Except String Json.Structured :=
  match json with
  | .arr elems => return .arr elems
  | .obj kvs => return .obj kvs
  | _ => throw "structured value expected"

instance : FromJson Json.Structured where
  fromJson? := structuredFromJson?

instance : ToJson Json.Structured where
  toJson
    | .arr elems => .arr elems
    | .obj kvs => .obj kvs

instance : FromStructured Json.Structured where
  fromStructured? params := .ok params

instance : ToStructured Json.Structured where
  toStructured params := params

end JsonRpc
end LeanWorker
