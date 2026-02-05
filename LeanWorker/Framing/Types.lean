module

public import Lean
public import LeanWorker.JsonRpc.Core

public section

namespace LeanWorker
namespace Framing

open Lean
open JsonRpc

structure Framing where
  encode : Json → ByteArray
  decode : ByteArray → Except Error (Array Json × ByteArray)

def framingError (message : String) : Error :=
  Error.withData Error.parseError (Json.str message)

end Framing
end LeanWorker
