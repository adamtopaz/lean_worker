module

public import LeanWorker
public import LeanWorker.Transport.Streams
public import LeanWorker.Transport.Logging
public import LeanWorkerTest.FullServer

public section

namespace LeanWorkerTest

open LeanWorker
open Std.Internal.IO.Async

def runServer : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let log ← LeanWorker.Transport.stderrLogger "SERVER"
  let transport ← Async.block <|
    LeanWorker.Transport.jsonTransportFromStreams stdin stdout .newline log
  let state ← Std.Mutex.new FullServer.defaultState
  Async.block <| FullServer.run transport (state := state)

end LeanWorkerTest

def main : IO Unit :=
  LeanWorkerTest.runServer
