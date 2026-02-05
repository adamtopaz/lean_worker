module

public import Lean
public import Std.Sync.Channel
public import Std.Internal.Async.Basic

public section

namespace LeanWorker
namespace Transport

open Std.Internal.IO.Async

inductive LogLevel where
  | debug
  | info
  | warn
  | error
deriving BEq, Repr, Inhabited

structure Transport (Incoming Outgoing : Type) where
  inbox : Std.CloseableChannel Incoming
  outbox : Std.CloseableChannel Outgoing
  log : LogLevel → String → IO Unit
  shutdown : Async Unit

abbrev ByteTransport := Transport ByteArray ByteArray

end Transport
end LeanWorker
