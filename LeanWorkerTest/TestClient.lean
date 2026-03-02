module

import LeanWorker.Client
import LeanWorker.Transport

namespace LeanWorkerTest

open Lean
open LeanWorker
open Client
open Transport
open JsonRpc
open Std.Internal.IO.Async

private abbrev SpawnedServer :=
  IO.Process.Child { stdin := .null, stdout := .piped, stderr := .inherit }

private def parseFrameSpec (args : List String) : Except String FrameSpec := do
  let mut selected? : Option FrameSpec := none
  for arg in args do
    match arg with
    | "--" =>
      pure ()
    | "--newline" =>
      match selected? with
      | none =>
        selected? := some .newline
      | some .newline =>
        pure ()
      | some .contentLength =>
        throw "cannot use both --newline and --header"
    | "--header" =>
      match selected? with
      | none =>
        selected? := some .contentLength
      | some .contentLength =>
        pure ()
      | some .newline =>
        throw "cannot use both --newline and --header"
    | _ =>
      throw s!"unknown flag: {arg}"
  return selected?.getD .newline

private def serverFrameArg (frameSpec : FrameSpec) : String :=
  match frameSpec with
  | .newline => "--newline"
  | .contentLength => "--header"

private def spawnTestServer (frameSpec : FrameSpec) : IO (SpawnedServer × IO.FS.Stream × IO.FS.Stream) := do
  let child ← IO.Process.spawn {
    cmd := "lake"
    args := #[("exe" : String), "test_server", "--", serverFrameArg frameSpec]
    stdin := .piped
    stdout := .piped
    stderr := .inherit
  }
  let (stdinHandle, child) ← child.takeStdin
  let stdin := IO.FS.Stream.ofHandle stdinHandle
  let stdout := IO.FS.Stream.ofHandle child.stdout
  return (child, stdin, stdout)

private def runEAsync (action : EAsync Error α) : IO (Except Error α) :=
  EIO.toIO' <| EAsync.block action

private def objParams (fields : List (String × Json)) : Json.Structured :=
  match Json.mkObj fields with
  | .obj kvs => .obj kvs
  | _ => .obj {}

private def runClientTest (frameSpec : FrameSpec) : IO Unit := do
  let (child, childStdin, childStdout) ← spawnTestServer frameSpec
  let transport ← Async.block <|
    clientTransportFromStreams childStdout childStdin frameSpec silentLogger
  let client ← Async.block <| getClient transport
  try
    let echoParams := objParams
      [
        ("message", Json.str "hello from test_client"),
        ("count", toJson (2 : Nat))
      ]
    let echoResult ← runEAsync <| client.request "echo" (some echoParams)
    match echoResult with
    | .ok result =>
      if result != toJson echoParams then
        throw <| IO.userError s!"unexpected echo result: {Json.compress result}"
    | .error err =>
      throw <| IO.userError s!"echo request failed: {err.code}: {err.message}"

    let missingResult ← runEAsync <| client.request "missingMethod" none
    match missingResult with
    | .ok _ =>
      throw <| IO.userError "expected method-not-found error"
    | .error err =>
      if err.code != Error.methodNotFound.code then
        throw <| IO.userError s!"unexpected error code: {err.code}"
  finally
    try
      child.kill
    catch _ =>
      pure ()
    try
      let _ ← child.wait
      pure ()
    catch _ =>
      pure ()
    try
      Async.block client.shutdown
    catch _ =>
      pure ()

end LeanWorkerTest

public def main (args : List String) : IO UInt32 := do
  match LeanWorkerTest.parseFrameSpec args with
  | .ok frameSpec =>
    try
      LeanWorkerTest.runClientTest frameSpec
      return 0
    catch err =>
      let stderr ← IO.getStderr
      stderr.putStrLn s!"error: {err}"
      return 1
  | .error message =>
    let stderr ← IO.getStderr
    stderr.putStrLn s!"error: {message}"
    stderr.putStrLn "usage: lake exe test_client -- [--newline | --header]"
    return 1
