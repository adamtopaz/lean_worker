module

public import LeanWorkerTest.Support
public import LeanWorkerTest.Tests.Support

public section

namespace LeanWorkerTest

open Lean
open LeanWorker.JsonRpc

def testMethodNotFound : IO Unit :=
  withManagedClient fun client => do
    let result ← runEAsync <| client.client.request "missing" none
    match result with
    | .ok _ => throw <| IO.userError "expected method not found error"
    | .error err =>
      assert (err.code == Error.methodNotFound.code) "unexpected error code"

def testInvalidParams : IO Unit :=
  withManagedClient fun client => do
    let params : Json.Structured := .arr #[Json.num 1]
    let result ← runEAsync <| client.client.request "add" (some params)
    match result with
    | .ok _ => throw <| IO.userError "expected invalid params error"
    | .error err =>
      assert (err.code == Error.invalidParams.code) "unexpected error code"

def testCustomError : IO Unit :=
  withManagedClient fun client => do
    let params := objParams
      [("code", Json.num 500), ("message", Json.str "boom"), ("data", Json.str "detail")]
    let result ← runEAsync <| client.client.request "error.custom" (some params)
    match result with
    | .ok _ => throw <| IO.userError "expected custom error"
    | .error err =>
      assert (err.code == 500) "unexpected custom error code"
      assert (err.message == "boom") "unexpected custom error message"
      assert (err.data? == some (Json.str "detail")) "unexpected custom error data"

def testInternalError : IO Unit :=
  withManagedClient fun client => do
    let result ← runEAsync <| client.client.request "error.internal" none
    match result with
    | .ok _ => throw <| IO.userError "expected internal error"
    | .error err =>
      assert (err.code == Error.internalError.code) "unexpected internal error code"

end LeanWorkerTest
