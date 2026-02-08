module

public import Lean
public import LeanWorker.JsonRpc.Core

public section

namespace LeanWorker
namespace Transport

open JsonRpc

structure Codec (Incoming Outgoing : Type) where
  decode : ByteArray → Except Error Incoming
  encode : Outgoing → ByteArray

end Transport
end LeanWorker
