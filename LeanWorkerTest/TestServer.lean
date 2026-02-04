module

public import LeanWorker
public import LeanWorker.Client
public import LeanWorkerTest.FullServer
public import LeanWorkerTest.Support

public section

namespace LeanWorkerTest

open LeanWorker
open Std.Internal.IO.Async

def runServer : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let log ← LeanWorker.Client.stderrLogger "SERVER"
  let byteTransport ← Async.block <|
    LeanWorkerTest.lineByteTransportFromStreams stdin stdout log
  let transport ← Async.block <|
    LeanWorker.Async.framedTransport byteTransport Framing.newline
  Async.block <| FullServer.run transport

end LeanWorkerTest

def main : IO Unit :=
  LeanWorkerTest.runServer
