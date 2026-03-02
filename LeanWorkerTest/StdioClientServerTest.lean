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
      let _ ← Async.block client.shutdown
      pure ()
    catch _ =>
      pure ()

def testSpawnStdioClientServer : IO Unit :=
  testSpawnStdioClientServerWithFraming "--serve" .newline

def testSpawnStdioClientServerContentLength : IO Unit :=
  testSpawnStdioClientServerWithFraming "--serve-content-length" .contentLength

private partial def waitForTaskFinish
    (task : AsyncTask α)
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
    let result ← Async.block <| await shutdownTask
    match result with
    | .ok _ =>
      pure ()
    | .error message =>
      throw <| IO.userError s!"graceful shutdown failed: {message}"

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
    let result ← Async.block <| await shutdownTask
    match result with
    | .ok _ =>
      pure ()
    | .error message =>
      throw <| IO.userError s!"fallback kill shutdown failed: {message}"

def testTransportShutdownFailureDoesNotBlock : IO Unit := do
  let cwd ← IO.currentDir
  let child ← IO.Process.spawn {
    cmd := "lake",
    args := #["exe", "stdio_client_server_test", "--sleep-forever"],
    cwd := some cwd,
    stdin := .piped,
    stdout := .piped,
    stderr := .inherit
  }
  let (stdinHandle, child) ← child.takeStdin
  let stdinStream := IO.FS.Stream.ofHandle stdinHandle
  let stdoutStream := IO.FS.Stream.ofHandle child.stdout
  let transport ← Async.block <|
    Transport.clientTransportFromStreams
      stdoutStream
      stdinStream
      .newline
      Transport.silentLogger
      (shutdownAction := do
        try
          child.kill
        catch _ =>
          pure ()
        return .error "forced shutdown failure for test")
  try
    let shutdownTask ← Async.block <| async transport.shutdown
    let finished ← waitForTaskFinish shutdownTask 100
    assert finished "transport shutdown should return even when shutdownAction fails"
    if finished then
      let result ← Async.block <| await shutdownTask
      match result with
      | .error _ =>
        pure ()
      | .ok _ =>
        throw <| IO.userError "transport shutdown should return an error on shutdownAction failure"
  finally
    try
      child.kill
    catch _ =>
      pure ()

def testClientShutdownCatchesTransportException : IO Unit := do
  let inbox : Std.CloseableChannel (Except Error Json) ← Std.CloseableChannel.new
  let outbox : Std.CloseableChannel Json ← Std.CloseableChannel.new
  let transport : Transport.ClientTransport :=
    {
      inbox := inbox,
      outbox := outbox,
      log := Transport.silentLogger,
      shutdown := do
        let _ ← inbox.close.toBaseIO
        throw <| IO.userError "forced transport shutdown exception"
    }
  let client ← Async.block <| Client.getClient transport
  let result ← Async.block client.shutdown
  match result with
  | .ok _ =>
    throw <| IO.userError "client shutdown should return an error when transport shutdown throws"
  | .error message =>
    assert (message.startsWith "transport shutdown threw exception:")
      s!"unexpected shutdown error message: {message}"

def testTransportWriterTimeoutReturnsError : IO Unit := do
  let readStream : IO.FS.Stream :=
    {
      flush := pure (),
      read := fun _ => pure ByteArray.empty,
      write := fun _ => pure (),
      getLine := pure "",
      putStr := fun _ => pure (),
      isTty := pure false
    }
  let writeStream : IO.FS.Stream :=
    {
      flush := pure (),
      read := fun _ => pure ByteArray.empty,
      write := fun _ => IO.sleep 1500,
      getLine := pure "",
      putStr := fun _ => pure (),
      isTty := pure false
    }
  let transport ← Async.block <|
    Transport.clientTransportFromStreams
      readStream
      writeStream
      .newline
      Transport.silentLogger
      (shutdownAction := pure (.ok ()))
  match ← Async.block <| await <| ← transport.outbox.send (Json.str "trigger writer") with
  | .ok _ =>
    pure ()
  | .error err =>
    throw <| IO.userError s!"failed to enqueue payload for writer-timeout test: {err}"
  let shutdownTask ← Async.block <| async transport.shutdown
  let finished ← waitForTaskFinish shutdownTask 200
  assert finished "transport shutdown should complete when writer timeout occurs"
  if finished then
    let result ← Async.block <| await shutdownTask
    match result with
    | .error message =>
      assert (message.startsWith "writer task did not finish before shutdown action")
        s!"unexpected writer-timeout shutdown message: {message}"
    | .ok _ =>
      throw <| IO.userError "expected transport shutdown to return error on writer timeout"

def testTransportWriterTimeoutStillRunsShutdownAction : IO Unit := do
  let shutdownRan : Std.Mutex Bool ← Std.Mutex.new false
  let readStream : IO.FS.Stream :=
    {
      flush := pure (),
      read := fun _ => pure ByteArray.empty,
      write := fun _ => pure (),
      getLine := pure "",
      putStr := fun _ => pure (),
      isTty := pure false
    }
  let writeStream : IO.FS.Stream :=
    {
      flush := pure (),
      read := fun _ => pure ByteArray.empty,
      write := fun _ => IO.sleep 1500,
      getLine := pure "",
      putStr := fun _ => pure (),
      isTty := pure false
    }
  let transport ← Async.block <|
    Transport.clientTransportFromStreams
      readStream
      writeStream
      .newline
      Transport.silentLogger
      (shutdownAction := do
        shutdownRan.atomically <| set true
        return .ok ())
  match ← Async.block <| await <| ← transport.outbox.send (Json.str "trigger writer") with
  | .ok _ =>
    pure ()
  | .error err =>
    throw <| IO.userError s!"failed to enqueue payload for writer-timeout shutdownAction test: {err}"
  let result ← Async.block transport.shutdown
  match result with
  | .error _ =>
    pure ()
  | .ok _ =>
    throw <| IO.userError "expected transport shutdown to return error on writer timeout"
  let ran ← shutdownRan.atomically get
  assert ran "shutdownAction should still run when writer shutdown fails"

def testTransportNewlinePartialFrameAtEofReturnsError : IO Unit := do
  let lines : Std.Mutex (List String) ← Std.Mutex.new ["{\"jsonrpc\":\"2.0\"", ""]
  let readStream : IO.FS.Stream :=
    {
      flush := pure (),
      read := fun _ => pure ByteArray.empty,
      write := fun _ => pure (),
      getLine := lines.atomically do
        match ← get with
        | [] => return ""
        | line :: rest =>
          set rest
          return line
      putStr := fun _ => pure (),
      isTty := pure false
    }
  let writeStream : IO.FS.Stream :=
    {
      flush := pure (),
      read := fun _ => pure ByteArray.empty,
      write := fun _ => pure (),
      getLine := pure "",
      putStr := fun _ => pure (),
      isTty := pure false
    }
  let transport ← Async.block <|
    Transport.clientTransportFromStreams
      readStream
      writeStream
      .newline
      Transport.silentLogger
  match ← Async.block <| await <| ← transport.inbox.recv with
  | some (.error err) =>
    assert (err.code == Error.parseError.code)
      s!"unexpected error code for partial newline frame: {err.code}"
  | some (.ok json) =>
    throw <| IO.userError s!"expected framing error for partial newline frame, got json: {Json.compress json}"
  | none =>
    throw <| IO.userError "expected framing error before inbox close"
  match ← Async.block <| await <| ← transport.inbox.recv with
  | none =>
    pure ()
  | some _ =>
    throw <| IO.userError "expected inbox to close after partial newline framing error"
  let shutdownResult ← Async.block transport.shutdown
  match shutdownResult with
  | .ok _ =>
    pure ()
  | .error message =>
    throw <| IO.userError s!"unexpected shutdown failure after newline EOF test: {message}"

end LeanWorkerTest
