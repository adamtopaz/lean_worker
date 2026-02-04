module

public import LeanWorker
public import LeanWorkerTest.FullServer.Types
public import LeanWorkerTest.FullServer.Handlers

public section

namespace LeanWorkerTest
namespace FullServer

open LeanWorker
open LeanWorker.JsonRpc
open LeanWorker.Server
open Std.Internal.IO.Async

def defaultContext : FullContext :=
  {
    name := "lean-worker-test",
    version := "1.0",
    defaultDelayMs := 25,
    allowCustomErrors := true
  }

def defaultState : FullState :=
  emptyState

def server
    (transport : Transport.Transport (Except Error Lean.Json) Lean.Json)
    : Server FullContext FullState :=
  {
    handlers := handlers,
    notifications := notifications,
    transport := transport
  }

def run
    (transport : Transport.Transport (Except Error Lean.Json) Lean.Json)
    (ctx : FullContext := defaultContext)
    (state : FullState := defaultState) : Async Unit :=
  Server.run (server transport) ctx state

end FullServer
end LeanWorkerTest
