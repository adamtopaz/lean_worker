module

import LeanWorker.Server
import LeanWorker.Transport

namespace LeanWorkerTest

open Lean
open LeanWorker
open Server
open Transport

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

open Std Internal IO Async in
private def runTestServer (frameSpec : FrameSpec) : Async Unit := do
  let stderr ← IO.getStderr
  let transport ← serverTransportFromStdio frameSpec fun lvl msg => do
    stderr.putStrLn s!"[{repr lvl}] {msg}"
  let server := testServer transport
  Server.run server () <| ← Std.Mutex.new ()

end LeanWorkerTest

public def main (args : List String) : IO UInt32 := do
  match LeanWorkerTest.parseFrameSpec args with
  | .ok frameSpec =>
    LeanWorkerTest.runTestServer frameSpec |>.block
    return 0
  | .error message =>
    let stderr ← IO.getStderr
    stderr.putStrLn s!"error: {message}"
    stderr.putStrLn "usage: lake exe test_server -- [--newline | --header]"
    return 1
