module

import LeanWorker.Client
import LeanWorker.Server
import LeanWorker.Transport
import LeanWorker.Framing
import Std.Internal.Async.TCP

namespace LeanWorkerTest

open Lean
open LeanWorker
open Client
open Server
open Transport
open Framing
open JsonRpc
open Std.Net
open Std.Internal.IO.Async

private def echo : StatelessHandler Unit Json.Structured (Option Json.Structured) :=
  fun param =>
    return param

private def handlers : HandlerRegistry Unit Unit :=
  .empty
    |>.addStateless "echo" echo

private def testServer (transport : Transport) : Server Unit Unit where
  handlers := handlers
  notifications := .empty
  transport := transport

private def parseFrameSpec (args : List String) : Except String Framing.Spec := do
  let mut selected? : Option Framing.Spec := none
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

private def runEAsync (action : EAsync Error α) : IO (Except Error α) :=
  EIO.toIO' <| EAsync.block action

private def objParams (fields : List (String × Json)) : Json.Structured :=
  match Json.mkObj fields with
  | .obj kvs => .obj kvs
  | _ => .obj {}

private def loopbackAddress (port : UInt16) : SocketAddress :=
  .v4 { addr := IPv4Addr.ofParts 127 0 0 1, port := port }

private def runClientAssertions (client : Client) : IO Unit := do
  let echoParams := objParams
    [
      ("message", Json.str "hello from test_tcp"),
      ("count", toJson (3 : Nat))
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

private def runTcpTest (framing : Framing.Spec) : IO Unit := do
  let serverSocket ← TCP.Socket.Server.mk
  serverSocket.bind (loopbackAddress 0)
  serverSocket.listen 16
  let localAddr ← serverSocket.getSockName
  let serverTask ← Async.block <| async do
    let socket ← serverSocket.accept
    let transport ← serverTransportFromAcceptedTcpSocket socket framing silentLogger
    let server := testServer transport
    Server.run server () <| ← Std.Mutex.new ()
  let transport ← Async.block <|
    clientTransportFromTcpAddress localAddr framing silentLogger
  let client ← Async.block <| getClient transport
  try
    runClientAssertions client
  finally
    Async.block client.shutdown
    Async.block <| await serverTask

end LeanWorkerTest

public def main (args : List String) : IO UInt32 := do
  match LeanWorkerTest.parseFrameSpec args with
  | .ok framing =>
    try
      LeanWorkerTest.runTcpTest framing
      return 0
    catch err =>
      let stderr ← IO.getStderr
      stderr.putStrLn s!"error: {err}"
      return 1
  | .error message =>
    let stderr ← IO.getStderr
    stderr.putStrLn s!"error: {message}"
    stderr.putStrLn "usage: lake exe test_tcp -- [--newline | --header]"
    return 1
