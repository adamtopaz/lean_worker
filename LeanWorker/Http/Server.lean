module

public import LeanWorker.Http.Types
public import LeanWorker.Transport.Tcp
public import LeanWorker.Async.Loops

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
  Transport.Tcp.listenByteTransport config.addr
    (backlog := config.backlog)
    (log := config.log)
    (handle := fun byteTransport => do
      let transport ← Async.framedTransport byteTransport (Framing.httpLike config.http)
      handle transport)

end Http
end LeanWorker
