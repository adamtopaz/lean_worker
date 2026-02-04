module

public import LeanWorker.Framing.HttpLike
public import LeanWorker.Transport.Types
public import Std.Net

public section

namespace LeanWorker
namespace Http

open Std.Net

def silentLogger : Transport.LogLevel → String → IO Unit :=
  fun _ _ => pure ()

structure ServerConfig where
  addr : SocketAddress
  backlog : UInt32 := 128
  http : Framing.HttpLikeConfig := {}
  log : Transport.LogLevel → String → IO Unit := silentLogger

structure ClientConfig where
  addr : SocketAddress
  http : Framing.HttpLikeConfig := {}
  log : Transport.LogLevel → String → IO Unit := silentLogger

end Http
end LeanWorker
