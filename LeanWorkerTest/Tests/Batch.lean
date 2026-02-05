module

public import LeanWorkerTest.Support
public import LeanWorkerTest.Tests.Support

public section

namespace LeanWorkerTest

open Lean
open LeanWorker.JsonRpc

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

end LeanWorkerTest
