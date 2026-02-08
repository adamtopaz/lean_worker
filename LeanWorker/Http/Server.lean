module

public import LeanWorker.Http.Types
public import LeanWorker.Transport.Tcp

public section

namespace LeanWorker
namespace Http

open JsonRpc
open Lean
open Std.Internal.IO.Async

def serve
    (config : ServerConfig)
    (handle : Transport.Transport (Except Error Lean.Json) Lean.Json → Async Unit) :
    Async Transport.Tcp.Listener := do
  Transport.Tcp.listenJsonTransport config.addr
    handle
    (.httpLike config.http)
    (backlog := config.backlog)
    (log := config.log)

end Http
end LeanWorker
