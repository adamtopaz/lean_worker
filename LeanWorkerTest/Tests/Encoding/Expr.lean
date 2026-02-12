module

public import LeanWorkerTest.Tests.Encoding.Support
public import LeanWorkerTest.Tests.Support

public section

namespace LeanWorkerTest

open Lean
open LeanWorker.Encoding

private def expectExprRoundTrip (env : Environment) (expr : Expr) (label : String) : IO Unit := do
  let didRoundTrip ← runMetaTestWithEnv env do
    let encoded ← encodeExpr expr
    match ← decodeExpr? encoded with
    | .ok decoded =>
      return Expr.equal expr decoded
    | .error _ =>
      return false
  assert didRoundTrip s!"expr binary round-trip failed for {label}"

private def expectExprDecodeError (bytes : ByteArray) : IO Unit := do
  match ← runMetaTest (decodeExpr? bytes) with
  | .ok _ =>
    throw <| IO.userError "expected expr decode failure"
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
