module

public import LeanWorker.Transport.Types

public section

namespace LeanWorker
namespace Transport

def logMessage (transport : Transport Incoming Outgoing)
    (level : LogLevel)
    (message : String) : IO Unit :=
  transport.log level message

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

end Transport
end LeanWorker
