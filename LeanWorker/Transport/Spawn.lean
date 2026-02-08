module

public import LeanWorker.Transport.Streams
public import LeanWorker.Transport.Logging

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

def spawnStdioTransportWithCodec
    (config : SpawnConfig)
    (frameSpec : FrameSpec)
    (codec : Codec Incoming Outgoing)
    (log : LogLevel → String → IO Unit := silentLogger) :
    Async (Transport (Except JsonRpc.Error Incoming) Outgoing) := do
  let child ← IO.Process.spawn (spawnArgs config)
  let (stdinHandle, child) ← child.takeStdin
  let stdinStream := IO.FS.Stream.ofHandle stdinHandle
  let stdoutStream := IO.FS.Stream.ofHandle child.stdout
  let shutdownAction : Async Unit := do
    let _ ← child.wait
    return
  Transport.transportFromStreams
    stdoutStream
    stdinStream
    frameSpec
    codec
    log
    shutdownAction

def spawnStdioTransport
    (config : SpawnConfig)
    (frameSpec : FrameSpec := .newline)
    (log : LogLevel → String → IO Unit := silentLogger) :
    Async (Transport (Except JsonRpc.Error Lean.Json) Lean.Json) :=
  spawnStdioTransportWithCodec config frameSpec JsonRpc.jsonCodec (log := log)

end Transport
end LeanWorker
