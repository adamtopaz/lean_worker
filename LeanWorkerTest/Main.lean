module

public import LeanWorkerTest.Tests.Support
public import LeanWorkerTest.StdioClientServerTest
public import LeanWorkerTest.Tests.JsonRpc
public import LeanWorkerTest.Tests.FramingCodec
public import LeanWorkerTest.Tests.Batch
public import LeanWorkerTest.Tests.Errors
public import LeanWorkerTest.Tests.State
public import LeanWorkerTest.Tests.Notifications

public section

def main : IO Unit := do
  LeanWorkerTest.runTest "spawn stdio client server" LeanWorkerTest.testSpawnStdioClientServer
  LeanWorkerTest.runTest "spawn stdio client server content-length" LeanWorkerTest.testSpawnStdioClientServerContentLength
  LeanWorkerTest.runTest "newline framing round trip" LeanWorkerTest.testNewlineFramingRoundTrip
  LeanWorkerTest.runTest "newline framing multiple" LeanWorkerTest.testNewlineFramingMultipleFrames
  LeanWorkerTest.runTest "newline framing partial" LeanWorkerTest.testNewlineFramingPartialFrame
  LeanWorkerTest.runTest "content-length framing round trip" LeanWorkerTest.testContentLengthFramingRoundTrip
  LeanWorkerTest.runTest "content-length framing partial" LeanWorkerTest.testContentLengthFramingPartialSecondFrame
  LeanWorkerTest.runTest "content-length missing header" LeanWorkerTest.testContentLengthFramingMissingHeader
  LeanWorkerTest.runTest "content-length with json codec" LeanWorkerTest.testContentLengthFramingWithJsonCodec
  LeanWorkerTest.runTest "content-length invalid payload" LeanWorkerTest.testContentLengthFramingWithJsonCodecInvalidPayload
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
