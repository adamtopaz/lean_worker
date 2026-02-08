module

public import LeanWorker.Http.Types
public import LeanWorker.Transport.Tcp
public import LeanWorker.Client

public section

namespace LeanWorker
namespace Http

open JsonRpc
open Lean
open Std.Internal.IO.Async

def connect (config : ClientConfig) : Async (Transport.Transport (Except Error Lean.Json) Lean.Json) := do
  Transport.Tcp.connectJsonTransport
    config.addr
    (.httpLike config.http)
    (log := config.log)

def client (config : ClientConfig) : Async LeanWorker.Client.Client := do
  let transport ← connect config
  LeanWorker.Client.getClient transport

end Http
end LeanWorker
