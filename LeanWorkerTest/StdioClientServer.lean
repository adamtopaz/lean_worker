module

public import LeanWorkerTest.StdioClientServerTest

public section

private partial def sleepForever : IO Unit := do
  IO.sleep 1000
  sleepForever

def main (args : List String) : IO UInt32 := do
  if let ["--serve"] := args then
    LeanWorkerTest.runStdioClientServer
  else if let ["--serve-content-length"] := args then
    LeanWorkerTest.runStdioClientServer .contentLength
  else if let ["--sleep-forever"] := args then
    sleepForever
  else
    LeanWorkerTest.runTest "spawn stdio client server" LeanWorkerTest.testSpawnStdioClientServer
  return 0
