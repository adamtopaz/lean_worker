module

public import LeanWorker
public import LeanWorkerTest.Tests.Support

public section

namespace LeanWorkerTest

open Lean
open LeanWorker
open LeanWorker.Encoding

private def expectDecodeError (input : String) : IO Unit := do
  match fromBase64? input with
  | .ok _ =>
    throw <| IO.userError s!"expected base64 decode failure for: {input}"
  | .error _ =>
    return ()

def testBase64KnownVectors : IO Unit := do
  let vectors : Array (String × String) :=
    #[
      ("", ""),
      ("f", "Zg=="),
      ("fo", "Zm8="),
      ("foo", "Zm9v"),
      ("foob", "Zm9vYg=="),
      ("fooba", "Zm9vYmE="),
      ("foobar", "Zm9vYmFy")
    ]

  for entry in vectors do
    let plain := entry.fst
    let encoded := entry.snd
    let plainBytes := plain.toUTF8
    let actualEncoded := toBase64 plainBytes
    assert (actualEncoded == encoded)
      s!"base64 encoding mismatch for '{plain}'"
    match fromBase64? encoded with
    | .ok decoded =>
      assert (decoded == plainBytes)
        s!"base64 decoding mismatch for '{encoded}'"
    | .error err =>
      throw <| IO.userError s!"base64 decode failed for '{encoded}': {err}"

def testBase64BinaryRoundTrip : IO Unit := do
  let bytes : ByteArray := ByteArray.mk #[0x00, 0x01, 0x7F, 0x80, 0xFE, 0xFF]
  let encoded := toBase64 bytes
  match fromBase64? encoded with
  | .ok decoded =>
    assert (decoded == bytes) "base64 failed to round-trip raw bytes"
  | .error err =>
    throw <| IO.userError s!"base64 decode failed for raw bytes: {err}"

def testBase64InvalidInputs : IO Unit := do
  expectDecodeError "A"
  expectDecodeError "AA-A"
  expectDecodeError "=AAA"
  expectDecodeError "AA=A"
  expectDecodeError "AA==AAAA"

def testBase64NonCanonicalPaddingBits : IO Unit := do
  expectDecodeError "Zh=="
  expectDecodeError "Zm9="

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
