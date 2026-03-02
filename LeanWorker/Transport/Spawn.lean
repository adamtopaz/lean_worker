module

public import LeanWorker.Transport.Streams
public import LeanWorker.Transport.Logging

public section

namespace LeanWorker
namespace Transport

open Std.Internal.IO.Async

private partial def waitForExitGracefully
    {cfg : IO.Process.StdioConfig}
    (child : IO.Process.Child cfg)
    (remainingPolls : Nat)
    (pollMs : UInt32 := 20) : Async Bool := do
  match ← child.tryWait with
  | some _ =>
    return true
  | none =>
    match remainingPolls with
    | 0 =>
      return false
    | remainingPolls + 1 =>
      IO.sleep pollMs
      waitForExitGracefully child remainingPolls pollMs

private def shutdownChild
    {cfg : IO.Process.StdioConfig}
    (child : IO.Process.Child cfg)
    (log : LogLevel → String → IO Unit)
    (timeoutMs? : Option Nat)
    (pollMs : UInt32 := 20)
    (postKillTimeoutMs : Nat := 500) : Async Unit := do
  match timeoutMs? with
  | none =>
    try
      let _ ← child.wait
      return
    catch _ =>
      return
  | some timeoutMs =>
    let pollNat := UInt32.toNat pollMs
    let polls := (timeoutMs + pollNat - 1) / pollNat
    let exited ← waitForExitGracefully child polls pollMs
    if exited then
      return
    else
      try
        child.kill
      catch _ =>
        logError log "failed to send terminate signal to child process"
      let postKillPolls := (postKillTimeoutMs + pollNat - 1) / pollNat
      let exitedAfterKill ← waitForExitGracefully child postKillPolls pollMs
      if !exitedAfterKill then
        logError log s!"child process did not exit within {postKillTimeoutMs}ms after terminate"

structure SpawnConfig where
  cmd : String
  args : Array String := #[]
  cwd : Option System.FilePath := none
  env : Array (String × Option String) := #[]
  inheritEnv : Bool := true
  stderr : IO.Process.Stdio := .inherit
  shutdownTimeoutMs? : Option Nat := some 1000

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

def spawnStdioClientTransport
    (config : SpawnConfig)
    (frameSpec : FrameSpec := .newline)
    (log : LogLevel → String → IO Unit := silentLogger) :
    Async ClientTransport := do
  let child ← IO.Process.spawn (spawnArgs config)
  let (stdinHandle, child) ← child.takeStdin
  let stdinStream := IO.FS.Stream.ofHandle stdinHandle
  let stdoutStream := IO.FS.Stream.ofHandle child.stdout
  let shutdownAction : Async Unit := do
    shutdownChild child log config.shutdownTimeoutMs?
  clientTransportFromStreams
    stdoutStream
    stdinStream
    frameSpec
    log
    shutdownAction

def spawnStdioServerTransport
    (config : SpawnConfig)
    (frameSpec : FrameSpec := .newline)
    (log : LogLevel → String → IO Unit := silentLogger) :
    Async ServerTransport := do
  let child ← IO.Process.spawn (spawnArgs config)
  let (stdinHandle, child) ← child.takeStdin
  let stdinStream := IO.FS.Stream.ofHandle stdinHandle
  let stdoutStream := IO.FS.Stream.ofHandle child.stdout
  let shutdownAction : Async Unit := do
    shutdownChild child log config.shutdownTimeoutMs?
  serverTransportFromStreams
    stdoutStream
    stdinStream
    frameSpec
    log
    shutdownAction

end Transport
end LeanWorker
