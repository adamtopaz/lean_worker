module

public import LeanWorker
public import LeanWorker.Http.Client
public import Std.Internal.Async.Basic

public section

namespace LeanWorkerTest

open Lean
open LeanWorker
open Std.Internal.IO.Async
def connect (addr : Std.Net.SocketAddress) : IO Client.Client := do
  let config : Http.ClientConfig := { addr := addr }
  Async.block <| Http.client config

end LeanWorkerTest
