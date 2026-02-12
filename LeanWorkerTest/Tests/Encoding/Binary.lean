module

public import LeanWorkerTest.Tests.Encoding.Support
public import LeanWorkerTest.Tests.Support

public section

namespace LeanWorkerTest

open LeanWorker.Encoding

def testToFromBinaryByteArray : IO Unit := do
  let bytes : ByteArray := ByteArray.mk #[0x00, 0x10, 0x7F, 0x80, 0xFF]
  assert (toBinary bytes == bytes) "ToBinary ByteArray should be identity"
  match fromBinary? (α := ByteArray) bytes with
  | .ok decoded =>
    assert (decoded == bytes) "FromBinary ByteArray should be identity"
  | .error err =>
    throw <| IO.userError s!"FromBinary ByteArray failed: {err}"

def testToFromBinaryString : IO Unit := do
  let text := "hello utf8"
  let bytes := toBinary text
  assert (bytes == text.toUTF8) "ToBinary String should use UTF-8 bytes"
  match fromBinary? (α := String) bytes with
  | .ok decoded =>
    assert (decoded == text) "FromBinary String did not restore original text"
  | .error err =>
    throw <| IO.userError s!"FromBinary String failed: {err}"

def testFromBinaryStringInvalidUtf8 : IO Unit := do
  let invalid : ByteArray := ByteArray.mk #[0xFF, 0xFE]
  match fromBinary? (α := String) invalid with
  | .ok _ =>
    throw <| IO.userError "expected invalid UTF-8 error"
  | .error _ =>
    return ()

end LeanWorkerTest
