module

public import LeanWorker
public import LeanWorker.Http.Server
public import LeanWorkerTest.FullServer

public section

namespace LeanWorkerTest

open LeanWorker
open Std.Internal.IO.Async

def serve (addr : Std.Net.SocketAddress) : Async Transport.Tcp.Listener := do
  let config : Http.ServerConfig := { addr := addr }
  Http.serve config fun transport =>
    FullServer.run transport

end LeanWorkerTest
