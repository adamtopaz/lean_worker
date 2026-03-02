module

public import LeanWorker
public import LeanWorker.Transport.Streams
public import LeanWorkerTest.Support
public import LeanWorkerTest.Tests.Support

public section

namespace LeanWorkerTest

open Lean
open LeanWorker
open LeanWorker.JsonRpc
open LeanWorker.Server
open Std.Internal.IO.Async

def echoHandler : StatelessHandler Unit Json.Structured (Option Json.Structured) :=
  fun params? =>
    return params?

def throwHandler : StatelessHandler Unit Json.Structured (Option Json.Structured) :=
  fun _ =>
    throw Error.internalError

def handlers : HandlerRegistry Unit Unit :=
  HandlerRegistry.empty
    |>.addStateless "echo" echoHandler
    |>.addStateless "throw" throwHandler

def runStdioClientServer (frameSpec : Transport.FrameSpec := .newline) : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let log ← Transport.stderrLogger "SERVER"
  let transport ← Async.block <|
    Transport.serverTransportFromStreams stdin stdout frameSpec log
  let server : Server Unit Unit :=
    {
      handlers := handlers,
      notifications := .empty,
      transport := transport
    }
  let state ← Std.Mutex.new ()
  Async.block <| Server.run server () state

def testSpawnStdioClientServerWithFraming
    (serveArg : String)
    (frameSpec : Transport.FrameSpec) : IO Unit := do
  let client ← Async.block <|
    Client.spawnStdioClient
      {
        cmd := "lake",
        args := #["exe", "stdio_client_server_test", serveArg]
      }
      frameSpec
  try
    let echoParams := objParams
      [
        ("message", Json.str "hello"),
        ("count", toJson (2 : Nat))
      ]
    let echoResult ← runEAsync <| client.request "echo" (some echoParams)
    match echoResult with
    | .ok result =>
      assert (result == toJson echoParams)
        s!"unexpected echo result: {Json.compress result}"
    | .error err =>
      throw <| IO.userError s!"echo request failed: {err.code}"

    let throwResult ← runEAsync <| client.request "throw" (some <| .arr #[])
    match throwResult with
    | .ok _ =>
      throw <| IO.userError "expected internal error response"
    | .error err =>
      assert (err.code == Error.internalError.code)
        s!"unexpected throw error code: {err.code}"
      assert (err.message == Error.internalError.message)
        s!"unexpected throw error message: {err.message}"
  finally
    try
      Async.block client.shutdown
    catch _ =>
      pure ()

def testSpawnStdioClientServer : IO Unit :=
  testSpawnStdioClientServerWithFraming "--serve" .newline

def testSpawnStdioClientServerContentLength : IO Unit :=
  testSpawnStdioClientServerWithFraming "--serve-content-length" .contentLength

private partial def waitForTaskFinish
    (task : AsyncTask Unit)
    (remainingPolls : Nat) : IO Bool := do
  match remainingPolls with
  | 0 =>
    return false
  | remainingPolls + 1 => do
    if (← task.getState) == .finished then
      return true
    IO.sleep 20
    waitForTaskFinish task remainingPolls

def testSpawnStdioClientGracefulShutdownCompletes : IO Unit := do
  let client ← Async.block <|
    Client.spawnStdioClient
      {
        cmd := "lake",
        args := #["exe", "stdio_client_server_test", "--serve"],
        shutdownTimeoutMs? := none
      }
      .newline
  let shutdownTask ← Async.block <| async client.shutdown
  let finished ← waitForTaskFinish shutdownTask 200
  assert finished "spawned stdio client graceful shutdown did not complete in time"
  if finished then
    Async.block <| await shutdownTask

def testSpawnStdioClientShutdownFallbackKill : IO Unit := do
  let client ← Async.block <|
    Client.spawnStdioClient
      {
        cmd := "lake",
        args := #["exe", "stdio_client_server_test", "--sleep-forever"],
        shutdownTimeoutMs? := some 20
      }
      .newline
  let shutdownTask ← Async.block <| async client.shutdown
  let finished ← waitForTaskFinish shutdownTask 250
  assert finished "spawned stdio client fallback kill shutdown did not complete in time"
  if finished then
    Async.block <| await shutdownTask

end LeanWorkerTest
