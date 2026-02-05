module

public import LeanWorkerTest.Tests.Support
public import LeanWorkerTest.Tests.JsonRpc
public import LeanWorkerTest.Tests.Batch
public import LeanWorkerTest.Tests.Errors
public import LeanWorkerTest.Tests.State
public import LeanWorkerTest.Tests.Notifications
public import LeanWorkerTest.Tests.Http

public section

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
