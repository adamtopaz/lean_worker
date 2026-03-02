module

public import LeanWorker
public import LeanWorker.Transport.Streams
public import LeanWorkerTest.FullServer
public import Std.Internal.Async.Basic

public section

namespace LeanWorkerTest

open Lean
open LeanWorker
open LeanWorker.JsonRpc
open LeanWorker.Server
open Std.Internal.IO.Async

inductive TransportMode where
  | stdio
deriving BEq

inductive FramingMode where
  | newline
  | contentLength
deriving BEq

structure CliConfig where
  transport : TransportMode := .stdio
  framing : FramingMode := .newline
  logLevel : Transport.LogLevel := .info
  maxTasks? : Option Nat := none
  name : String := FullServer.defaultContext.name
  version : String := FullServer.defaultContext.version
  delayMs : Nat := FullServer.defaultContext.defaultDelayMs
  allowCustomErrors : Bool := FullServer.defaultContext.allowCustomErrors

def usage : String :=
  String.intercalate "\n"
    [
      "Usage: full_server [options]",
      "",
      "Options:",
      "  --transport stdio",
      "  --framing newline|content-length",
      "  --log-level debug|info|warn|error",
      "  --max-tasks <n>",
      "  --name <string>",
      "  --version <string>",
      "  --delay-ms <n>",
      "  --allow-custom-errors | --no-custom-errors",
      "  --help"
    ]

def logLevelTag : Transport.LogLevel → String
  | .debug => "DEBUG"
  | .info => "INFO"
  | .warn => "WARN"
  | .error => "ERROR"

def logLevelRank : Transport.LogLevel → Nat
  | .debug => 0
  | .info => 1
  | .warn => 2
  | .error => 3

def stderrLogger
    (label : String)
    (minLevel : Transport.LogLevel) : IO (Transport.LogLevel → String → IO Unit) := do
  let stream ← IO.getStderr
  return fun level message => do
    if logLevelRank level >= logLevelRank minLevel then
      stream.putStrLn s!"[{label} {logLevelTag level}] {message}"
      stream.flush
    else
      pure ()

def parseTransportMode (value : String) : Except String TransportMode :=
  match value.toLower with
  | "stdio" => return .stdio
  | _ => throw s!"invalid transport: {value}"

def parseFramingMode (value : String) : Except String FramingMode :=
  match value.toLower with
  | "newline" => return .newline
  | "content-length" => return .contentLength
  | "contentlength" => return .contentLength
  | _ => throw s!"invalid framing: {value}"

def parseLogLevel (value : String) : Except String Transport.LogLevel :=
  match value.toLower with
  | "debug" => return .debug
  | "info" => return .info
  | "warn" => return .warn
  | "error" => return .error
  | _ => throw s!"invalid log level: {value}"

def parseNat (value : String) (label : String) : Except String Nat :=
  match value.toNat? with
  | some n => return n
  | none => throw s!"invalid {label}: {value}"

def parseArgsAux (cfg : CliConfig) : List String → Except String CliConfig
  | [] => return cfg
  | "--transport" :: value :: rest => do
    let transport ← parseTransportMode value
    parseArgsAux { cfg with transport := transport } rest
  | "--framing" :: value :: rest => do
    let framing ← parseFramingMode value
    parseArgsAux { cfg with framing := framing } rest
  | "--log-level" :: value :: rest => do
    let logLevel ← parseLogLevel value
    parseArgsAux { cfg with logLevel := logLevel } rest
  | "--max-tasks" :: value :: rest => do
    let maxTasks ← parseNat value "max-tasks"
    parseArgsAux { cfg with maxTasks? := some maxTasks } rest
  | "--name" :: value :: rest =>
    parseArgsAux { cfg with name := value } rest
  | "--version" :: value :: rest =>
    parseArgsAux { cfg with version := value } rest
  | "--delay-ms" :: value :: rest => do
    let delayMs ← parseNat value "delay-ms"
    parseArgsAux { cfg with delayMs := delayMs } rest
  | "--allow-custom-errors" :: rest =>
    parseArgsAux { cfg with allowCustomErrors := true } rest
  | "--no-custom-errors" :: rest =>
    parseArgsAux { cfg with allowCustomErrors := false } rest
  | "--help" :: _ =>
    throw usage
  | flag :: [] =>
    throw s!"missing value for {flag}"
  | flag :: _ =>
    throw s!"unknown option: {flag}"

def parseArgs (args : List String) : Except String CliConfig :=
  parseArgsAux {} args

def frameSpecFromMode : FramingMode → Transport.FrameSpec
  | .newline => .newline
  | .contentLength => .contentLength

def buildContext (cfg : CliConfig) : FullServer.FullContext :=
  {
    name := cfg.name,
    version := cfg.version,
    defaultDelayMs := cfg.delayMs,
    allowCustomErrors := cfg.allowCustomErrors
  }

def runServer
    (cfg : CliConfig)
    (transport : LeanWorker.Transport.ServerTransport)
    (state : Std.Mutex FullServer.FullState) : Async Unit := do
  let server : Server FullServer.FullContext FullServer.FullState :=
    { FullServer.server transport with maxTasks := cfg.maxTasks? }
  Server.run server (buildContext cfg) state

def runStdio (cfg : CliConfig) (log : Transport.LogLevel → String → IO Unit) : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let frameSpec := frameSpecFromMode cfg.framing
  let transport ← Async.block <|
    LeanWorker.Transport.serverTransportFromStreams stdin stdout frameSpec log
  let state ← Std.Mutex.new FullServer.defaultState
  Async.block <| runServer cfg transport state

def main (args : List String) : IO UInt32 := do
  if args.contains "--help" then
    let stdout ← IO.getStdout
    stdout.putStrLn usage
    stdout.flush
    return 0
  match parseArgs args with
  | .error message =>
    let stderr ← IO.getStderr
    stderr.putStrLn message
    stderr.flush
    return 1
  | .ok cfg =>
    let log ← stderrLogger "FULLSERVER" cfg.logLevel
    match cfg.transport with
    | .stdio =>
      runStdio cfg log
      return 0

end LeanWorkerTest

def main (args : List String) : IO UInt32 :=
  LeanWorkerTest.main args
