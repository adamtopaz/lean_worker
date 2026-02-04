module

public import LeanWorker
public import LeanWorker.Client
public import LeanWorker.Transport.Tcp
public import LeanWorker.Framing.Newline
public import LeanWorker.Framing.ContentLength
public import LeanWorker.Framing.HttpLike
public import LeanWorkerTest.FullServer
public import LeanWorkerTest.Support
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
  | tcp
deriving BEq

inductive FramingMode where
  | newline
  | contentLength
  | httpLike
deriving BEq

structure CliConfig where
  transport : TransportMode := .stdio
  framing : FramingMode := .newline
  host : String := "127.0.0.1"
  port : UInt16 := 41000
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
      "  --transport stdio|tcp",
      "  --framing newline|content-length|http-like",
      "  --host <host> (tcp only)",
      "  --port <port> (tcp only)",
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
  | "tcp" => return .tcp
  | _ => throw s!"invalid transport: {value}"

def parseFramingMode (value : String) : Except String FramingMode :=
  match value.toLower with
  | "newline" => return .newline
  | "content-length" => return .contentLength
  | "contentlength" => return .contentLength
  | "http-like" => return .httpLike
  | "httplike" => return .httpLike
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

def parsePort (value : String) : Except String UInt16 := do
  let n ← parseNat value "port"
  if n <= 65535 then
    return UInt16.ofNat n
  else
    throw s!"port out of range: {value}"

def parseOctet (value : String) : Except String UInt8 := do
  let n ← parseNat value "ipv4 octet"
  if n <= 255 then
    return UInt8.ofNat n
  else
    throw s!"invalid ipv4 octet: {value}"

def parseHost (value : String) : Except String Std.Net.IPv4Addr := do
  if value == "localhost" then
    return Std.Net.IPv4Addr.ofParts 127 0 0 1
  match value.splitOn "." with
  | [a, b, c, d] =>
    let a ← parseOctet a
    let b ← parseOctet b
    let c ← parseOctet c
    let d ← parseOctet d
    return Std.Net.IPv4Addr.ofParts a b c d
  | _ => throw s!"invalid host: {value}"

def parseArgsAux (cfg : CliConfig) : List String → Except String CliConfig
  | [] => return cfg
  | "--transport" :: value :: rest => do
    let transport ← parseTransportMode value
    parseArgsAux { cfg with transport := transport } rest
  | "--framing" :: value :: rest => do
    let framing ← parseFramingMode value
    parseArgsAux { cfg with framing := framing } rest
  | "--host" :: value :: rest =>
    parseArgsAux { cfg with host := value } rest
  | "--port" :: value :: rest => do
    let port ← parsePort value
    parseArgsAux { cfg with port := port } rest
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

def framingFromMode : FramingMode → LeanWorker.Framing.Framing
  | .newline => LeanWorker.Framing.newline
  | .contentLength => LeanWorker.Framing.contentLength
  | .httpLike =>
    LeanWorker.Framing.httpLike
      {
        startLine := "HTTP/1.1 200 OK",
        headers := [("Content-Type", "application/json")]
      }

def buildContext (cfg : CliConfig) : FullServer.FullContext :=
  {
    name := cfg.name,
    version := cfg.version,
    defaultDelayMs := cfg.delayMs,
    allowCustomErrors := cfg.allowCustomErrors
  }

def runServer
    (cfg : CliConfig)
    (transport : Transport.Transport (Except Error Json) Json)
    (state : Std.Mutex FullServer.FullState) : Async Unit := do
  let server : Server FullServer.FullContext FullServer.FullState :=
    { FullServer.server transport with maxTasks := cfg.maxTasks? }
  Server.run server (buildContext cfg) state

def runStdio (cfg : CliConfig) (log : Transport.LogLevel → String → IO Unit) : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let byteTransport ← Async.block <|
    match cfg.framing with
    | .newline =>
      LeanWorkerTest.lineByteTransportFromStreams stdin stdout log
    | .contentLength =>
      LeanWorker.Client.byteTransportFromStreams stdin stdout log
    | .httpLike =>
      LeanWorker.Client.byteTransportFromStreams stdin stdout log
  let framing := framingFromMode cfg.framing
  let transport ← Async.block <|
    LeanWorker.Async.framedTransport byteTransport framing
  let state ← Std.Mutex.new FullServer.defaultState
  Async.block <| runServer cfg transport state

partial def waitLoop : IO Unit := do
  IO.sleep 1000
  waitLoop

def runTcp (cfg : CliConfig) (log : Transport.LogLevel → String → IO Unit) : IO Unit := do
  let addr ←
    match parseHost cfg.host with
    | .ok host =>
      pure <| Std.Net.SocketAddress.v4 { addr := host, port := cfg.port }
    | .error message =>
      throw <| IO.userError message
  let framing := framingFromMode cfg.framing
  let sharedState ← Std.Mutex.new FullServer.defaultState
  let handle : Transport.ByteTransport → Async Unit := fun byteTransport => do
    let transport ← LeanWorker.Async.framedTransport byteTransport framing
    runServer cfg transport sharedState
  let listener ← Async.block <|
    Transport.Tcp.listenByteTransport addr handle (log := log)
  log .info s!"listening on {cfg.host}:{cfg.port}"
  try
    waitLoop
  finally
    Async.block listener.shutdown

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
    | .tcp =>
      runTcp cfg log
      return 0

end LeanWorkerTest

def main (args : List String) : IO UInt32 :=
  LeanWorkerTest.main args
