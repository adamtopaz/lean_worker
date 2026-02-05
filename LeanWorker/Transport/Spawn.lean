module

public import LeanWorker.Transport.Line
public import LeanWorker.Transport.Logging
public import LeanWorker.Async.Loops
public import LeanWorker.Framing.Newline
public import LeanWorker.JsonRpc.Core

public section

namespace LeanWorker
namespace Transport

open Lean
open JsonRpc
open Std.Internal.IO.Async

structure SpawnConfig where
  cmd : String
  args : Array String := #[]
  cwd : Option System.FilePath := none
  env : Array (String × Option String) := #[]
  inheritEnv : Bool := true
  stderr : IO.Process.Stdio := .inherit

abbrev spawnArgs (config : SpawnConfig) : IO.Process.SpawnArgs :=
  {
    cmd := config.cmd,
    args := config.args,
    cwd := config.cwd,
    env := config.env,
    inheritEnv := config.inheritEnv,
    stdin := .piped,
    stdout := .piped,
    stderr := config.stderr
  }

def spawnStdioTransport
    (config : SpawnConfig)
    (log : LogLevel → String → IO Unit := silentLogger) :
    Async (Transport (Except Error Json) Json) := do
  let child ← IO.Process.spawn (spawnArgs config)
  let (stdinHandle, child) ← child.takeStdin
  let stdinStream := IO.FS.Stream.ofHandle stdinHandle
  let stdoutStream := IO.FS.Stream.ofHandle child.stdout
  let shutdownAction : Async Unit := do
    let _ ← child.wait
    return
  let byteTransport ←
    lineByteTransportFromStreams stdoutStream stdinStream log shutdownAction
  LeanWorker.Async.framedTransport byteTransport Framing.newline

end Transport
end LeanWorker
