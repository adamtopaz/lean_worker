module

public import LeanWorkerTest.Support
public import LeanWorkerTest.Tests.Support
public import LeanWorkerTest.FullServer
public import Std.Internal.Async.Basic

public section

namespace LeanWorkerTest

open Lean
open LeanWorker.JsonRpc
open Std.Internal.IO.Async

def testInfo : IO Unit :=
  withManagedClient fun client => do
    let result ← runEAsync <| client.client.request "info" none
    match result with
    | .ok json =>
      let name ← jsonToString (← expectObjField json "name")
      let version ← jsonToString (← expectObjField json "version")
      let delay ← jsonToNat (← expectObjField json "defaultDelayMs")
      assert (name == FullServer.defaultContext.name) "unexpected info name"
      assert (version == FullServer.defaultContext.version) "unexpected info version"
      assert (delay == FullServer.defaultContext.defaultDelayMs) "unexpected info delay"
    | .error err =>
      throw <| IO.userError s!"info failed: {err.code}"

def testEcho : IO Unit :=
  withManagedClient fun client => do
    let params := objParams [("key", Json.str "value"), ("count", Json.num 2)]
    let result ← runEAsync <| client.client.request "echo" (some params)
    match result with
    | .ok json =>
      let key ← jsonToString (← expectObjField json "key")
      let count ← jsonToNat (← expectObjField json "count")
      assert (key == "value") "unexpected echo key"
      assert (count == 2) "unexpected echo count"
    | .error err =>
      throw <| IO.userError s!"echo failed: {err.code}"

def testSum : IO Unit :=
  withManagedClient fun client => do
    let params : Json.Structured := .arr #[Json.num 1, Json.num 2, Json.num 3]
    let result ← runEAsync <| client.client.request "sum" (some params)
    match result with
    | .ok json =>
      assert (json == Json.num 6) "unexpected sum result"
    | .error err =>
      throw <| IO.userError s!"sum failed: {err.code}"

def testCounterState : IO Unit :=
  withManagedClient fun client => do
    let reset ← runEAsync <| client.client.request "counter.reset" none
    match reset with
    | .ok json =>
      assert (json == Json.num 0) "unexpected counter reset value"
    | .error err =>
      throw <| IO.userError s!"counter reset failed: {err.code}"
    let addParams := objParams [("delta", Json.num 5)]
    let addResult ← runEAsync <| client.client.request "counter.add" (some addParams)
    match addResult with
    | .ok json =>
      assert (json == Json.num 5) "unexpected counter add value"
    | .error err =>
      throw <| IO.userError s!"counter add failed: {err.code}"
    let count ← runEAsync <| client.client.request "counter.get" none
    match count with
    | .ok json =>
      assert (json == Json.num 5) "unexpected counter get value"
    | .error err =>
      throw <| IO.userError s!"counter get failed: {err.code}"

def testCounterAddNegative : IO Unit :=
  withManagedClient fun client => do
    let params := objParams [("delta", toJson (-1 : Int))]
    let result ← runEAsync <| client.client.request "counter.add" (some params)
    match result with
    | .ok _ => throw <| IO.userError "expected invalid params for negative delta"
    | .error err =>
      assert (err.code == Error.invalidParams.code) "unexpected error code"

def testCounterConcurrentAdds : IO Unit :=
  withManagedClient fun client => do
    let params := objParams [("delta", Json.num 1)]
    let result ← EIO.toIO' <| EAsync.block do
      let t1 ← EAsync.async <| client.client.request "counter.add" (some params)
      let t2 ← EAsync.async <| client.client.request "counter.add" (some params)
      let r1 ← EAsync.await t1
      let r2 ← EAsync.await t2
      return (r1, r2)
    match result with
    | .ok (r1, r2) =>
      assert (r1 == Json.num 1 || r2 == Json.num 1) "missing counter value 1"
      assert (r1 == Json.num 2 || r2 == Json.num 2) "missing counter value 2"
    | .error err =>
      throw <| IO.userError s!"counter concurrent failed: {err.code}"
    let final ← runEAsync <| client.client.request "counter.get" none
    match final with
    | .ok json =>
      assert (json == Json.num 2) "unexpected final counter value"
    | .error err =>
      throw <| IO.userError s!"counter get failed: {err.code}"

def testKvOperations : IO Unit :=
  withManagedClient fun client => do
    let key := "alpha"
    let value := Json.str "one"
    let setParams := objParams [("key", Json.str key), ("value", value)]
    let setResult ← runEAsync <| client.client.request "kv.set" (some setParams)
    match setResult with
    | .ok json =>
      let hadValue ← jsonToBool (← expectObjField json "hadValue")
      assert (hadValue == false) "unexpected initial kv.set result"
      let kvs ← expectObj json
      assert (kvs.get? "oldValue" == none) "unexpected oldValue in kv.set"
    | .error err =>
      throw <| IO.userError s!"kv.set failed: {err.code}"
    let getParams := objParams [("key", Json.str key)]
    let getResult ← runEAsync <| client.client.request "kv.get" (some getParams)
    match getResult with
    | .ok json =>
      let found ← jsonToBool (← expectObjField json "found")
      assert (found == true) "expected kv.get to find value"
      let fetched ← expectObjField json "value"
      assert (fetched == value) "unexpected kv.get value"
    | .error err =>
      throw <| IO.userError s!"kv.get failed: {err.code}"
    let update := objParams [("key", Json.str key), ("value", Json.str "two")]
    let updateResult ← runEAsync <| client.client.request "kv.set" (some update)
    match updateResult with
    | .ok json =>
      let hadValue ← jsonToBool (← expectObjField json "hadValue")
      assert (hadValue == true) "expected kv.set to report old value"
      let oldValue ← expectObjField json "oldValue"
      assert (oldValue == value) "unexpected kv.set old value"
    | .error err =>
      throw <| IO.userError s!"kv.set update failed: {err.code}"
    let deleteResult ← runEAsync <| client.client.request "kv.delete" (some getParams)
    match deleteResult with
    | .ok json =>
      assert (json == Json.bool true) "expected kv.delete to return true"
    | .error err =>
      throw <| IO.userError s!"kv.delete failed: {err.code}"
    let missing ← runEAsync <| client.client.request "kv.get" (some getParams)
    match missing with
    | .ok json =>
      let found ← jsonToBool (← expectObjField json "found")
      assert (found == false) "expected kv.get to be missing"
    | .error err =>
      throw <| IO.userError s!"kv.get missing failed: {err.code}"

def testParallelSleep : IO Unit :=
  withManagedClient fun client => do
    let ms := 120
    let params1 := sleepParams ms (some "a")
    let params2 := sleepParams ms (some "b")
    let start ← monoMsNow
    let result ← EIO.toIO' <| EAsync.block do
      let t1 ← EAsync.async <| client.client.request "sleep" (some params1)
      let t2 ← EAsync.async <| client.client.request "sleep" (some params2)
      let r1 ← EAsync.await t1
      let r2 ← EAsync.await t2
      return (r1, r2)
    let stop ← monoMsNow
    let elapsed := stop - start
    match result with
    | .ok (r1, r2) =>
      let ms1 ← jsonToNat (← expectObjField r1 "ms")
      let ms2 ← jsonToNat (← expectObjField r2 "ms")
      assert (ms1 == ms) "unexpected sleep ms"
      assert (ms2 == ms) "unexpected sleep ms"
      let tag1 ← jsonToString (← expectObjField r1 "tag")
      let tag2 ← jsonToString (← expectObjField r2 "tag")
      assert ((tag1 == "a" && tag2 == "b") || (tag1 == "b" && tag2 == "a"))
        "unexpected sleep tags"
      assert (elapsed < ms + 80) "expected concurrent sleep timing"
    | .error err =>
      throw <| IO.userError s!"sleep failed: {err.code}"

end LeanWorkerTest
