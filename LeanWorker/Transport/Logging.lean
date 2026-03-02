module

public import LeanWorker.Transport.Types
public import LeanWorker.JsonRpc.Encoding
public import Std.Sync.Channel
public import Std.Internal.Async.Basic

public section

namespace LeanWorker
namespace Transport

open Lean
open JsonRpc
open Std.Internal.IO.Async

def logMessage
    (log : LogLevel → String → IO Unit)
    (level : LogLevel)
    (message : String) : IO Unit :=
  log level message

def logLevelTag : LogLevel → String
  | .debug => "DEBUG"
  | .info => "INFO"
  | .warn => "WARN"
  | .error => "ERROR"

def stderrLogger (label : String) : IO (LogLevel → String → IO Unit) := do
  let stream ← IO.getStderr
  return fun level message => do
    stream.putStrLn s!"[{label} {logLevelTag level}] {message}"
    stream.flush

def silentLogger : LogLevel → String → IO Unit := fun _ _ => pure ()

def errorToString (err : Error) : String :=
  s!"{err.code}: {err.message}"

def logAsync
    (log : LogLevel → String → IO Unit)
    (level : LogLevel)
    (message : String) : Async Unit :=
  EAsync.lift (log level message)

def logError
    (log : LogLevel → String → IO Unit)
    (message : String) : Async Unit :=
  logAsync log (show LogLevel from .error) message

def sendOrLog
    (log : LogLevel → String → IO Unit)
    (label : String)
    (channel : Std.CloseableChannel α)
    (value : α) : Async Bool := do
  match ← await <| ← channel.send value with
  | .ok _ => return true
  | .error err =>
    logError log s!"{label} send failed: {err}"
    return false

def closeOrLog
    (log : LogLevel → String → IO Unit)
    (label : String)
    (channel : Std.CloseableChannel α) : Async Unit := do
  match ← channel.close.toBaseIO with
  | .ok _ => return
  | .error err =>
    logError log s!"{label} close failed: {err}"

end Transport
end LeanWorker
