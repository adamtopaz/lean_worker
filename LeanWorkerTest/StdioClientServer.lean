module

public import LeanWorkerTest.StdioClientServerTest

public section

def main (args : List String) : IO UInt32 := do
  if let ["--serve"] := args then
    LeanWorkerTest.runStdioClientServer
  else if let ["--serve-content-length"] := args then
    LeanWorkerTest.runStdioClientServer .contentLength
  else
    LeanWorkerTest.runTest "spawn stdio client server" LeanWorkerTest.testSpawnStdioClientServer
  return 0
