module

public import LeanWorker
public import LeanWorker.Client
public import Std.Internal.Async.Basic

public section

namespace LeanWorkerTest

open Lean
open LeanWorker
open Std.Internal.IO.Async

abbrev RawChild := IO.Process.Child { stdin := .null, stdout := .piped, stderr := .inherit }

structure RawClient where
  child : RawChild
  stdin : IO.FS.Stream
  stdout : IO.FS.Stream

def spawnRawClient : IO RawClient := do
  let cwd ← IO.currentDir
  let serverPath := cwd / ".lake" / "build" / "bin" / "test_server"
  let child ← IO.Process.spawn {
    cmd := serverPath.toString,
    args := #[],
    stdin := .piped,
    stdout := .piped,
    stderr := .inherit
  }
  let (stdinHandle, child) ← child.takeStdin
  let stdin := IO.FS.Stream.ofHandle stdinHandle
  let stdout := IO.FS.Stream.ofHandle child.stdout
  return { child, stdin, stdout }

def killRawClient (client : RawClient) : IO Unit := do
  client.child.kill

def sendLine (client : RawClient) (line : String) : IO Unit := do
  client.stdin.putStr (line ++ "\n")
  client.stdin.flush

def sendJson (client : RawClient) (json : Json) : IO Unit :=
  sendLine client (Json.compress json)

def readJson (client : RawClient) : IO Json := do
  let line := (← client.stdout.getLine).trimAsciiEnd.toString
  if line.isEmpty then
    throw <| IO.userError "unexpected EOF from server"
  match Json.parse line with
  | .ok json => return json
  | .error err => throw <| IO.userError s!"invalid JSON from server: {err}"

partial def lineReadLoop
    (readStream : IO.FS.Stream)
    (log : Transport.LogLevel → String → IO Unit)
    (inbox : Std.CloseableChannel ByteArray) : Async Unit := do
  let line := (← readStream.getLine)
  if line.isEmpty then
    LeanWorker.Async.closeOrLog log "byte inbox" inbox
  else
    let sent ← LeanWorker.Async.sendOrLog log "byte inbox" inbox line.toUTF8
    if sent then
      lineReadLoop readStream log inbox

partial def lineWriteLoop
    (writeStream : IO.FS.Stream)
    (log : Transport.LogLevel → String → IO Unit)
    (outbox : Std.CloseableChannel ByteArray) : Async Unit := do
  match ← await <| ← outbox.recv with
  | none => return
  | some bytes =>
    writeStream.write bytes
    writeStream.flush
    lineWriteLoop writeStream log outbox

def lineByteTransportFromStreams
    (readStream writeStream : IO.FS.Stream)
    (log : Transport.LogLevel → String → IO Unit)
    (shutdownAction : Async Unit := pure ()) : Async Transport.ByteTransport := do
  let inbox : Std.CloseableChannel ByteArray ← Std.CloseableChannel.new
  let outbox : Std.CloseableChannel ByteArray ← Std.CloseableChannel.new
  let readLoop : Async Unit := do
    try
      lineReadLoop readStream log inbox
    catch err =>
      LeanWorker.Async.logError log s!"byte read error: {err}"
      LeanWorker.Async.closeOrLog log "byte inbox" inbox
  let writeLoop : Async Unit := do
    try
      lineWriteLoop writeStream log outbox
    catch err =>
      LeanWorker.Async.logError log s!"byte write error: {err}"
  let readerTask : AsyncTask Unit ← async readLoop
  let writerTask : AsyncTask Unit ← async writeLoop
  let shutdown : Async Unit := do
    await readerTask
    await writerTask
    shutdownAction
  return { inbox, outbox, log, shutdown }

structure ManagedClient where
  client : LeanWorker.Client.Client
  child : RawChild

def spawnManagedClient : IO ManagedClient := do
  let raw ← spawnRawClient
  let log ← LeanWorker.Client.stderrLogger "TESTCLIENT"
  let byteTransport ← Async.block <|
    lineByteTransportFromStreams raw.stdout raw.stdin log
  let transport ← Async.block <|
    LeanWorker.Async.framedTransport byteTransport Framing.newline
  let client ← Async.block <| LeanWorker.Client.getClient transport
  return { client, child := raw.child }

def shutdownManagedClient (client : ManagedClient) : IO Unit := do
  client.child.kill
  try
    Async.block client.client.shutdown
  catch _ =>
    pure ()

def withRawClient (action : RawClient → IO α) : IO α := do
  let client ← spawnRawClient
  try
    action client
  finally
    killRawClient client

def withManagedClient (action : ManagedClient → IO α) : IO α := do
  let client ← spawnManagedClient
  try
    action client
  finally
    shutdownManagedClient client

def runEAsync (action : EAsync Error α) : IO (Except Error α) :=
  EIO.toIO' <| EAsync.block action

end LeanWorkerTest
