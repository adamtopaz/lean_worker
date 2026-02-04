module

public import LeanWorker
public import LeanWorkerTest.Support
public import LeanWorkerTest.HttpSupport
public import LeanWorkerTest.FullServer

public section

namespace LeanWorkerTest

open Lean
open LeanWorker
open LeanWorker.JsonRpc
open Std.Internal.IO.Async

def assert (cond : Bool) (message : String) : IO Unit :=
  if cond then
    return
  else
    throw <| IO.userError message

def runTest (name : String) (action : IO Unit) : IO Unit := do
  let err ← IO.getStderr
  err.putStrLn s!"[TEST] {name}"
  err.flush
  action
  err.putStrLn s!"[OK] {name}"
  err.flush

def expectErrorResponse (json : Json) (code : Int) (id : RpcId) : IO Unit := do
  match fromJson? (α := Response) json with
  | .ok (.error respId err) =>
    assert (respId == id) s!"unexpected id: {Json.compress (toJson respId)}"
    assert (err.code == code) s!"unexpected error code: {err.code}"
  | .ok _ =>
    throw <| IO.userError "expected error response"
  | .error msg =>
    throw <| IO.userError s!"failed to decode response: {msg}"

def expectResultResponse (json : Json) (id : RpcId) : IO Json := do
  match fromJson? (α := Response) json with
  | .ok (.result respId result) =>
    assert (respId == id) s!"unexpected id: {Json.compress (toJson respId)}"
    return result
  | .ok _ =>
    throw <| IO.userError "expected result response"
  | .error msg =>
    throw <| IO.userError s!"failed to decode response: {msg}"

def decodeResponses (json : Json) : IO (Array Response) := do
  match json with
  | .arr items =>
    items.mapM fun item =>
      match fromJson? (α := Response) item with
      | .ok response => return response
      | .error msg => throw <| IO.userError s!"invalid response in batch: {msg}"
  | _ => throw <| IO.userError "expected batch response array"

def hasError (responses : Array Response) (code : Int) (id : RpcId) : Bool :=
  responses.any fun response =>
    match response with
    | .error respId err => respId == id && err.code == code
    | _ => false

def findResult (responses : Array Response) (id : RpcId) : Option Json :=
  responses.findSome? fun response =>
    match response with
    | .result respId result => if respId == id then some result else none
    | _ => none

def objParams (fields : List (String × Json)) : Json.Structured :=
  match Json.mkObj fields with
  | .obj kvs => .obj kvs
  | _ => .obj {}

def expectObj (json : Json) : IO (Std.TreeMap.Raw String Json compare) := do
  match json with
  | .obj kvs => return kvs
  | _ => throw <| IO.userError "expected object"

def expectObjField (json : Json) (key : String) : IO Json := do
  let kvs ← expectObj json
  match kvs.get? key with
  | some value => return value
  | none => throw <| IO.userError s!"missing field: {key}"

def jsonToNat (json : Json) : IO Nat := do
  match json.getInt? with
  | .ok value =>
    if value < 0 then
      throw <| IO.userError "expected non-negative integer"
    else
      return value.toNat
  | .error msg => throw <| IO.userError msg

def jsonToString (json : Json) : IO String := do
  match json.getStr? with
  | .ok value => return value
  | .error msg => throw <| IO.userError msg

def jsonToBool (json : Json) : IO Bool := do
  match json with
  | .bool value => return value
  | _ => throw <| IO.userError "expected boolean"

def sleepMs (ms : Nat) : IO Unit :=
  IO.sleep (UInt32.ofNat ms)

def monoMsNow : IO Nat := do
  IO.monoMsNow

def sleepParams (ms : Nat) (tag? : Option String := none) : Json.Structured :=
  let fields := match tag? with
    | none => [("ms", toJson ms)]
    | some tag => [("ms", toJson ms), ("tag", Json.str tag)]
  objParams fields

partial def waitForSnapshot
    (client : Client.Client)
    (attempts : Nat)
    (delayMs : Nat)
    (predicate : Json → IO Bool) : IO Json := do
  let rec loop : Nat → IO Json
    | 0 => throw <| IO.userError "timeout waiting for state snapshot"
    | remaining => do
      let result ← runEAsync <| client.request "state.snapshot" none
      match result with
      | .ok json =>
        if (← predicate json) then
          return json
        else
          sleepMs delayMs
          loop (remaining - 1)
      | .error err =>
        throw <| IO.userError s!"snapshot failed: {err.code}"
  loop attempts

def testParseError : IO Unit :=
  withRawClient fun client => do
    sendLine client "{\"jsonrpc\": \"2.0\", \"method\": \"ping\","
    let response ← readJson client
    expectErrorResponse response (-32700) RpcId.null

def testInvalidRequest : IO Unit :=
  withRawClient fun client => do
    sendLine client "{\"jsonrpc\": \"2.0\", \"id\": 1}"
    let response ← readJson client
    expectErrorResponse response (-32600) RpcId.null

def testEmptyBatch : IO Unit :=
  withRawClient fun client => do
    sendLine client "[]"
    let response ← readJson client
    expectErrorResponse response (-32600) RpcId.null

def testBatchInvalidItem : IO Unit :=
  withRawClient fun client => do
    let request : Json := toJson ({ id := RpcId.num 1, method := "ping", params? := none } : Request)
    sendJson client (Json.arr #[Json.num 1, request])
    let response ← readJson client
    let responses ← decodeResponses response
    assert (responses.size == 2) "expected two responses"
    assert (hasError responses (-32600) RpcId.null) "missing invalid request error"
    match findResult responses (RpcId.num 1) with
    | some result =>
      assert (result == Json.str "pong") "unexpected ping result"
    | none => throw <| IO.userError "missing ping result"

def testBatchNotification : IO Unit :=
  withRawClient fun client => do
    let notification : Json :=
      toJson ({ method := "notify.bump", params? := none } : Notification)
    let request : Json :=
      toJson ({ id := RpcId.num 1, method := "ping", params? := none } : Request)
    sendJson client (Json.arr #[notification, request])
    let response ← readJson client
    let responses ← decodeResponses response
    assert (responses.size == 1) "expected single response for batch with notification"
    match responses.toList with
    | [response] =>
      match response with
      | Response.result respId result =>
        assert (respId == RpcId.num 1) "unexpected id in notification batch"
        assert (result == Json.str "pong") "unexpected ping result"
      | Response.error _ _ =>
        throw <| IO.userError "unexpected error response in notification batch"
    | _ =>
      throw <| IO.userError "missing response in notification batch"

def testBatchOrdering : IO Unit :=
  withRawClient fun client => do
    let req1 : Json := toJson ({ id := RpcId.num 1, method := "ping", params? := none } : Request)
    let addParams : Json.Structured := objParams [("a", Json.num 1), ("b", Json.num 2)]
    let req2 : Json := toJson ({ id := RpcId.num 2, method := "add", params? := some addParams } : Request)
    sendJson client (Json.arr #[req1, req2])
    let response ← readJson client
    let responses ← decodeResponses response
    assert (responses.size == 2) "expected two responses"
    match findResult responses (RpcId.num 1) with
    | some result => assert (result == Json.str "pong") "unexpected ping result"
    | none => throw <| IO.userError "missing ping response"
    match findResult responses (RpcId.num 2) with
    | some result => assert (result == Json.num 3) "unexpected add result"
    | none => throw <| IO.userError "missing add response"

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

def testNotifications : IO Unit :=
  withManagedClient fun client => do
    let bumpParams := objParams [("delta", Json.num 2)]
    let logParams := objParams [("message", Json.str "hello")]
    let bumpResult ← runEAsync <| client.client.notify "notify.bump" (some bumpParams)
    match bumpResult with
    | .ok _ => pure ()
    | .error err => throw <| IO.userError s!"notify.bump failed: {err.code}"
    let logResult ← runEAsync <| client.client.notify "notify.log" (some logParams)
    match logResult with
    | .ok _ => pure ()
    | .error err => throw <| IO.userError s!"notify.log failed: {err.code}"
    let snapshot ← waitForSnapshot client.client 20 10 fun json => do
      let counter ← jsonToNat (← expectObjField json "counter")
      let eventsSize ← jsonToNat (← expectObjField json "eventsSize")
      return counter >= 2 && eventsSize >= 1
    let counter ← jsonToNat (← expectObjField snapshot "counter")
    let eventsSize ← jsonToNat (← expectObjField snapshot "eventsSize")
    assert (counter == 2) "unexpected counter after notifications"
    assert (eventsSize == 1) "unexpected events size after notifications"

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

def testHttpPing : IO Unit :=
  LeanWorkerTest.withHttpServer fun port =>
    LeanWorkerTest.withHttpClient port fun client => do
      let result ← LeanWorkerTest.runEAsync <| client.request "ping" none
      match result with
      | .ok json =>
        assert (json == Json.str "pong") "unexpected http ping result"
      | .error err =>
        throw <| IO.userError s!"http ping failed: {err.code}"

def testHttpConcurrent : IO Unit :=
  LeanWorkerTest.withHttpServer fun port =>
    LeanWorkerTest.withHttpClient port fun client => do
      let params : Json.Structured := objParams [("a", Json.num 2), ("b", Json.num 5)]
      let result ← EIO.toIO' <| EAsync.block do
        let t1 ← EAsync.async <| client.request "ping" none
        let t2 ← EAsync.async <| client.request "add" (some params)
        let r1 ← EAsync.await t1
        let r2 ← EAsync.await t2
        return (r1, r2)
      match result with
      | .ok (r1, r2) =>
        assert (r1 == Json.str "pong") "unexpected concurrent ping result"
        assert (r2 == Json.num 7) "unexpected concurrent add result"
      | .error err =>
        throw <| IO.userError s!"http concurrent failed: {err.code}"

def testHttpInvalidJson : IO Unit :=
  LeanWorkerTest.withHttpServer fun port =>
    LeanWorkerTest.withRawHttp port fun transport => do
      let request := LeanWorkerTest.httpRequest "{bad json"
      Async.block <| LeanWorkerTest.sendRawHttp transport request
      let response ← Async.block <| LeanWorkerTest.recvHttpJson transport
      match response with
      | .ok json =>
        expectErrorResponse json (-32700) RpcId.null
      | .error err =>
        throw <| IO.userError s!"expected response, got transport error: {err.code}"

def testHttpMalformedHeader : IO Unit :=
  LeanWorkerTest.withHttpServer fun port =>
    LeanWorkerTest.withRawHttp port fun transport => do
      let request := "POST / HTTP/1.1\r\n\r\n{}"
      Async.block <| LeanWorkerTest.sendRawHttp transport request
      let response ← Async.block <| LeanWorkerTest.recvHttpJson transport
      match response with
      | .ok json =>
        expectErrorResponse json (-32700) RpcId.null
      | .error err =>
        throw <| IO.userError s!"expected response, got transport error: {err.code}"

def testHttpClosedConnection : IO Unit :=
  LeanWorkerTest.withHttpServer fun port =>
    LeanWorkerTest.withRawHttp port fun transport => do
      let request := LeanWorkerTest.httpRequest "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}"
      Async.block <| LeanWorkerTest.sendRawHttp transport request
      match ← transport.outbox.close.toBaseIO with
      | .ok _ => return
      | .error _ => return

end LeanWorkerTest

def main : IO Unit := do
  LeanWorkerTest.runTest "parse error" LeanWorkerTest.testParseError
  LeanWorkerTest.runTest "invalid request" LeanWorkerTest.testInvalidRequest
  LeanWorkerTest.runTest "empty batch" LeanWorkerTest.testEmptyBatch
  LeanWorkerTest.runTest "batch invalid item" LeanWorkerTest.testBatchInvalidItem
  LeanWorkerTest.runTest "batch with notification" LeanWorkerTest.testBatchNotification
  LeanWorkerTest.runTest "batch ordering" LeanWorkerTest.testBatchOrdering
  LeanWorkerTest.runTest "method not found" LeanWorkerTest.testMethodNotFound
  LeanWorkerTest.runTest "invalid params" LeanWorkerTest.testInvalidParams
  LeanWorkerTest.runTest "info" LeanWorkerTest.testInfo
  LeanWorkerTest.runTest "echo" LeanWorkerTest.testEcho
  LeanWorkerTest.runTest "sum" LeanWorkerTest.testSum
  LeanWorkerTest.runTest "counter state" LeanWorkerTest.testCounterState
  LeanWorkerTest.runTest "counter negative" LeanWorkerTest.testCounterAddNegative
  LeanWorkerTest.runTest "counter concurrent" LeanWorkerTest.testCounterConcurrentAdds
  LeanWorkerTest.runTest "kv operations" LeanWorkerTest.testKvOperations
  LeanWorkerTest.runTest "notifications" LeanWorkerTest.testNotifications
  LeanWorkerTest.runTest "custom error" LeanWorkerTest.testCustomError
  LeanWorkerTest.runTest "internal error" LeanWorkerTest.testInternalError
  LeanWorkerTest.runTest "parallel sleep" LeanWorkerTest.testParallelSleep
  LeanWorkerTest.runTest "http ping" LeanWorkerTest.testHttpPing
  LeanWorkerTest.runTest "http concurrent" LeanWorkerTest.testHttpConcurrent
  LeanWorkerTest.runTest "http invalid json" LeanWorkerTest.testHttpInvalidJson
  LeanWorkerTest.runTest "http malformed header" LeanWorkerTest.testHttpMalformedHeader
  LeanWorkerTest.runTest "http closed connection" LeanWorkerTest.testHttpClosedConnection
