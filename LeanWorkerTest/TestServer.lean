module

public import LeanWorker
public import LeanWorker.Transport.Line
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
  let byteTransport ← Async.block <|
    LeanWorker.Transport.lineByteTransportFromStreams stdin stdout log
  let transport ← Async.block <|
    LeanWorker.Async.framedTransport byteTransport Framing.newline
  let state ← Std.Mutex.new FullServer.defaultState
  Async.block <| FullServer.run transport (state := state)

end LeanWorkerTest

def main : IO Unit :=
  LeanWorkerTest.runServer
