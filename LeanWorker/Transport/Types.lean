module

public import Lean
public import Std.Sync.Channel
public import Std.Internal.Async.Basic
public import LeanWorker.JsonRpc.Core

public section

namespace LeanWorker
namespace Transport

open Std.Internal.IO.Async
open JsonRpc

inductive LogLevel where
  | debug
  | info
  | warn
  | error
deriving BEq, Repr, Inhabited

structure ServerTransport where
  inbox : Std.CloseableChannel (Except Error Lean.Json)
  outbox : Std.CloseableChannel Lean.Json
  log : LogLevel → String → IO Unit
  shutdown : Async Unit

structure ClientTransport where
  inbox : Std.CloseableChannel (Except Error Lean.Json)
  outbox : Std.CloseableChannel Lean.Json
  log : LogLevel → String → IO Unit
  shutdown : Async Unit

end Transport
end LeanWorker
