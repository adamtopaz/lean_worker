module

public import LeanWorkerTest.HttpSupport
public import LeanWorkerTest.Support
public import LeanWorkerTest.Tests.Support
public import Std.Internal.Async.Basic

public section

namespace LeanWorkerTest

open Lean
open LeanWorker.JsonRpc
open Std.Internal.IO.Async

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
