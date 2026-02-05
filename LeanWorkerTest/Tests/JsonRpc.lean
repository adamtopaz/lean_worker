module

public import LeanWorkerTest.Support
public import LeanWorkerTest.Tests.Support

public section

namespace LeanWorkerTest

open LeanWorker.JsonRpc

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

end LeanWorkerTest
