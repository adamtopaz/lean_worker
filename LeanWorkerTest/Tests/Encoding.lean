module

public import LeanWorker
public import LeanWorkerTest.Tests.Support

public section

namespace LeanWorkerTest

open Lean
open LeanWorker
open LeanWorker.Encoding

private def runMetaWithCore (env : Environment) (action : MetaM α) : IO α := do
  let ctx : Core.Context := {
    fileName := "<encoding-tests>",
    fileMap := default
  }
  let st : Core.State := {
    env := env
  }
  (action.run').toIO' ctx st

private def runMetaTest (action : MetaM α) : IO α := do
  runMetaWithCore (← mkEmptyEnvironment) action

private def runMetaTestWithEnv (env : Environment) (action : MetaM α) : IO α := do
  runMetaWithCore env action

private def getConstInfoOrThrow (env : Environment) (declName : Name) : IO ConstantInfo := do
  match env.find? declName with
  | some info =>
    return info
  | none =>
    throw <| IO.userError s!"missing declaration in imported environment: {declName}"

private def getConstValueOrThrow (info : ConstantInfo) : IO Expr := do
  match info.value? with
  | some value =>
    return value
  | none =>
    throw <| IO.userError s!"declaration does not have a value: {info.name}"

private def expectExprRoundTrip (env : Environment) (expr : Expr) (label : String) : IO Unit := do
  let didRoundTrip ← runMetaTestWithEnv env do
    let encoded ← encodeExpr expr
    match ← decodeExpr? encoded with
    | .ok decoded =>
      return Expr.equal expr decoded
    | .error _ =>
      return false
  assert didRoundTrip s!"expr binary round-trip failed for {label}"

private def importInitEnv : IO Environment := do
  let sysroot ← Lean.findSysroot
  Lean.initSearchPath sysroot
  Lean.importModules #[`Init] {}

private def expectExprDecodeError (bytes : ByteArray) : IO Unit := do
  match ← runMetaTest (decodeExpr? bytes) with
  | .ok _ =>
    throw <| IO.userError "expected expr decode failure"
  | .error _ =>
    return ()

private def setByteIfPresent (bytes : ByteArray) (index : Nat) (value : UInt8) : ByteArray :=
  if index < bytes.size then
    bytes.set! index value
  else
    bytes

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

def testExprBinaryRoundTrip : IO Unit := do
  let didRoundTrip ← runMetaTest do
    Meta.withLocalDeclD `x (mkConst ``Nat) fun x => do
      let u ← Meta.mkFreshLevelMVar
      let m ← Meta.mkFreshExprMVar (some (mkSort u))

      let level : Level :=
        .imax
          (.max (.succ (.param `uParam)) u)
          .zero

      let pos0 : String.Pos.Raw := { byteIdx := 0 }
      let pos1 : String.Pos.Raw := { byteIdx := 1 }
      let pos2 : String.Pos.Raw := { byteIdx := 2 }
      let pos3 : String.Pos.Raw := { byteIdx := 3 }

      let leading : Substring.Raw := {
        str := " ab",
        startPos := pos0,
        stopPos := pos1
      }
      let trailing : Substring.Raw := {
        str := "cd ",
        startPos := pos2,
        stopPos := pos3
      }
      let rawVal : Substring.Raw := {
        str := "x",
        startPos := pos0,
        stopPos := pos1
      }

      let info : SourceInfo := .original leading pos1 trailing pos2
      let stx : Syntax :=
        .node info `Lean.Parser.Term.ident #[(
          .ident
            (.synthetic pos0 pos1 true)
            rawVal
            `x
            [
              .namespace `Lean,
              .decl ``Nat.succ ["toString"]
            ]
        ), .atom .none "token", .missing]

      let metadata : MData := {
        entries := [
          (`kString, DataValue.ofString "payload"),
          (`kBool, DataValue.ofBool true),
          (`kName, DataValue.ofName `Lean),
          (`kNat, DataValue.ofNat 42),
          (`kInt, DataValue.ofInt (-7)),
          (`kSyntax, DataValue.ofSyntax stx)
        ]
      }

      let eBVar : Expr := .bvar 0
      let eFVar : Expr := .fvar x.fvarId!
      let eMVar : Expr := .mvar m.mvarId!
      let eSort : Expr := .sort level
      let eConst : Expr := .const ``Nat.succ [level]
      let eApp : Expr := .app eConst eFVar
      let eLam : Expr := .lam `y eSort (.app eBVar (.lit (.natVal 7))) .implicit
      let eForall : Expr := .forallE `z eSort (.app eBVar (.lit (.strVal "text"))) .instImplicit
      let eLet : Expr := .letE `w eSort eApp (.app eMVar eFVar) true
      let eProj : Expr := .proj ``Prod 0 eFVar
      let expr : Expr :=
        .mdata metadata <|
          .app (.app (.app (.app (.app eLam eForall) eLet) eProj) eApp) eMVar

      let encoded ← encodeExpr expr
      match ← decodeExpr? encoded with
      | .ok decoded =>
        return Expr.equal expr decoded
      | .error _ =>
        return false

  assert didRoundTrip "expr binary codec failed to round-trip expression"

def testExprBinaryRoundTripInitEnvTypes : IO Unit := do
  let env ← importInitEnv
  let natAdd ← getConstInfoOrThrow env ``Nat.add
  let natRec ← getConstInfoOrThrow env ``Nat.rec
  expectExprRoundTrip env natAdd.type "`Nat.add` type"
  expectExprRoundTrip env natRec.type "`Nat.rec` type"

def testExprBinaryRoundTripInitEnvValues : IO Unit := do
  let env ← importInitEnv
  let natAdd ← getConstInfoOrThrow env ``Nat.add
  let natSub ← getConstInfoOrThrow env ``Nat.sub
  let natAddValue ← getConstValueOrThrow natAdd
  let natSubValue ← getConstValueOrThrow natSub
  expectExprRoundTrip env natAddValue "`Nat.add` value"
  expectExprRoundTrip env natSubValue "`Nat.sub` value"

def testExprBinaryDecodeUnknownFVar : IO Unit := do
  let expr : Expr := .fvar ⟨`missingFVar⟩
  let encoded ← runMetaTest (encodeExpr expr)
  expectExprDecodeError encoded

def testExprBinaryDecodeUnknownMVar : IO Unit := do
  let expr : Expr := .mvar ⟨`missingMVar⟩
  let encoded ← runMetaTest (encodeExpr expr)
  expectExprDecodeError encoded

def testExprBinaryDecodeUnknownLMVar : IO Unit := do
  let expr : Expr := .sort (.mvar ⟨`missingLMVar⟩)
  let encoded ← runMetaTest (encodeExpr expr)
  expectExprDecodeError encoded

def testExprBinaryInvalidMagic : IO Unit := do
  let encoded ← runMetaTest (encodeExpr (.lit (.natVal 10)))
  let corrupted := setByteIfPresent encoded 0 (UInt8.ofNat 0)
  expectExprDecodeError corrupted

def testExprBinaryInvalidVersion : IO Unit := do
  let encoded ← runMetaTest (encodeExpr (.lit (.natVal 11)))
  let corrupted := setByteIfPresent encoded 4 (UInt8.ofNat 255)
  expectExprDecodeError corrupted

def testExprBinaryInvalidTag : IO Unit := do
  let encoded ← runMetaTest (encodeExpr (.lit (.natVal 12)))
  let corrupted := setByteIfPresent encoded 5 (UInt8.ofNat 255)
  expectExprDecodeError corrupted

def testExprBinaryTrailingData : IO Unit := do
  let encoded ← runMetaTest (encodeExpr (.lit (.natVal 13)))
  expectExprDecodeError (encoded.push (UInt8.ofNat 0))

end LeanWorkerTest
