module

public import Lean

public section

namespace LeanWorker
namespace Encoding

class ToBinary (α : Type) where
  toBinary : α → ByteArray

export ToBinary (toBinary)

class FromBinary (α : Type) where
  fromBinary? : ByteArray → Except String α

export FromBinary (fromBinary?)

instance : ToBinary ByteArray where
  toBinary bytes := bytes

instance : FromBinary ByteArray where
  fromBinary? bytes := .ok bytes

instance : ToBinary String where
  toBinary text := text.toUTF8

instance : FromBinary String where
  fromBinary? bytes :=
    match String.fromUTF8? bytes with
    | some text => .ok text
    | none => .error "invalid UTF-8 payload"

end Encoding
end LeanWorker
