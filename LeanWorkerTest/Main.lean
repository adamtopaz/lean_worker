module

public import LeanWorkerTest.Tests.Support
public import LeanWorkerTest.StdioClientServerTest
public import LeanWorkerTest.Tests.JsonRpc
public import LeanWorkerTest.Tests.FramingCodec
public import LeanWorkerTest.Tests.Encoding
public import LeanWorkerTest.Tests.Batch
public import LeanWorkerTest.Tests.Errors
public import LeanWorkerTest.Tests.State
public import LeanWorkerTest.Tests.Notifications
public import LeanWorkerTest.Tests.Http

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
  LeanWorkerTest.runTest "http-like framing round trip" LeanWorkerTest.testHttpLikeFramingRoundTrip
  LeanWorkerTest.runTest "http-like missing start line" LeanWorkerTest.testHttpLikeFramingMissingStartLine
  LeanWorkerTest.runTest "json codec round trip" LeanWorkerTest.testJsonCodecRoundTrip
  LeanWorkerTest.runTest "json codec invalid utf8" LeanWorkerTest.testJsonCodecInvalidUtf8
  LeanWorkerTest.runTest "json codec invalid json" LeanWorkerTest.testJsonCodecInvalidJson
  LeanWorkerTest.runTest "base64 known vectors" LeanWorkerTest.testBase64KnownVectors
  LeanWorkerTest.runTest "base64 binary round trip" LeanWorkerTest.testBase64BinaryRoundTrip
  LeanWorkerTest.runTest "base64 invalid inputs" LeanWorkerTest.testBase64InvalidInputs
  LeanWorkerTest.runTest "base64 canonical padding" LeanWorkerTest.testBase64NonCanonicalPaddingBits
  LeanWorkerTest.runTest "binary bytearray instances" LeanWorkerTest.testToFromBinaryByteArray
  LeanWorkerTest.runTest "binary string instances" LeanWorkerTest.testToFromBinaryString
  LeanWorkerTest.runTest "binary invalid utf8" LeanWorkerTest.testFromBinaryStringInvalidUtf8
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
  LeanWorkerTest.runTest "http ping" LeanWorkerTest.testHttpPing
  LeanWorkerTest.runTest "http concurrent" LeanWorkerTest.testHttpConcurrent
  LeanWorkerTest.runTest "http invalid json" LeanWorkerTest.testHttpInvalidJson
  LeanWorkerTest.runTest "http malformed header" LeanWorkerTest.testHttpMalformedHeader
  LeanWorkerTest.runTest "http closed connection" LeanWorkerTest.testHttpClosedConnection
