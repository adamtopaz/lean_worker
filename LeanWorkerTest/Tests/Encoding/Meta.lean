module

public import LeanWorkerTest.Tests.Encoding.Support
public import LeanWorkerTest.Tests.Support

public section

namespace LeanWorkerTest

open Lean
open LeanWorker.Encoding

private def expectMetaContextDecodeError (env : Environment) (bytes : ByteArray) : IO Unit := do
  match ← runCoreWithEnv env (decodeMetaContext? bytes) with
  | .ok _ =>
    throw <| IO.userError "expected meta context decode failure"
  | .error _ =>
    return ()

private def expectMetaStateDecodeError (env : Environment) (bytes : ByteArray) : IO Unit := do
  match ← runCoreWithEnv env (decodeMetaState? bytes) with
  | .ok _ =>
    throw <| IO.userError "expected meta state decode failure"
  | .error _ =>
    return ()

private def mkMetaSemanticFixture
    : IO (Environment × Meta.Context × Meta.State × FVarId × MVarId × LMVarId) := do
  let env ← mkEmptyEnvironment
  let data ← runMetaWithCore env do
    Meta.withLocalDeclD `x (mkSort .zero) fun x => do
      let level ← Meta.mkFreshLevelMVar
      let .mvar lmvarId := level
        | throwError "expected fresh universe metavariable"
      let mvar ← Meta.mkFreshExprMVar (some (mkSort level))
      modify fun st => {
        st with
        zetaDeltaFVarIds := st.zetaDeltaFVarIds.insert x.fvarId!
        postponed := st.postponed.push {
          ref := Syntax.missing,
          lhs := level,
          rhs := .zero,
          ctx? := none
        }
      }
      let ctx ← read
      let st ← get
      return (ctx, st, x.fvarId!, mvar.mvarId!, lmvarId)
  let (ctx, st, fvarId, mvarId, lmvarId) := data
  return (env, ctx, st, fvarId, mvarId, lmvarId)

def testMetaContextStateSemanticRoundTrip : IO Unit := do
  let (env, ctx, st, fvarId, mvarId, lmvarId) ← mkMetaSemanticFixture

  let encodedCtxResult ← runCoreWithEnv env (encodeMetaContext? ctx)
  let encodedCtx ←
    match encodedCtxResult with
    | .ok bytes =>
      pure bytes
    | .error err =>
      throw <| IO.userError s!"meta context encode failed: {err}"

  let encodedState ← runCoreWithEnv env (encodeMetaState st)

  let decodedCtxResult ← runCoreWithEnv env (decodeMetaContext? encodedCtx)
  let decodedCtx ←
    match decodedCtxResult with
    | .ok decoded =>
      pure decoded
    | .error err =>
      throw <| IO.userError s!"meta context decode failed: {err}"

  let decodedStateResult ← runCoreWithEnv env (decodeMetaState? encodedState)
  let decodedState ←
    match decodedStateResult with
    | .ok decoded =>
      pure decoded
    | .error err =>
      throw <| IO.userError s!"meta state decode failed: {err}"

  let didPreserveSemantics ← runCoreWithEnv env <|
    Meta.MetaM.run' (do
      let hasFVar := (← getLCtx).find? fvarId |>.isSome
      let mctx ← getMCtx
      let hasMVar := (mctx.findDecl? mvarId).isSome
      let hasLMVar := (mctx.findLevelDepth? lmvarId).isSome
      let metaState ← getThe Meta.State
      let postponedSize := metaState.postponed.size
      let cacheIsEmpty := metaState.cache.inferType.isEmpty
      let diagIsEmpty := metaState.diag.unfoldCounter.isEmpty
      return hasFVar && hasMVar && hasLMVar && postponedSize == st.postponed.size && cacheIsEmpty && diagIsEmpty
    ) decodedCtx decodedState

  assert didPreserveSemantics "meta context/state semantic round-trip failed"

def testMetaContextEncodeRejectsCanUnfold : IO Unit := do
  let env ← mkEmptyEnvironment
  let ctx ← runMetaWithCore env do
    Meta.withCanUnfoldPred (fun _ _ => pure true) do
      read

  match ← runCoreWithEnv env (encodeMetaContext? ctx) with
  | .ok _ =>
    throw <| IO.userError "expected meta context encoding failure when `canUnfold?` is set"
  | .error _ =>
    return ()

def testMetaContextInvalidMagic : IO Unit := do
  let (env, ctx, _, _, _, _) ← mkMetaSemanticFixture
  let encodedResult ← runCoreWithEnv env (encodeMetaContext? ctx)
  let encoded ←
    match encodedResult with
    | .ok bytes =>
      pure bytes
    | .error err =>
      throw <| IO.userError s!"meta context encode failed: {err}"
  let corrupted := setByteIfPresent encoded 0 (UInt8.ofNat 0)
  expectMetaContextDecodeError env corrupted

def testMetaStateInvalidMagic : IO Unit := do
  let (env, _, st, _, _, _) ← mkMetaSemanticFixture
  let encoded ← runCoreWithEnv env (encodeMetaState st)
  let corrupted := setByteIfPresent encoded 0 (UInt8.ofNat 0)
  expectMetaStateDecodeError env corrupted

end LeanWorkerTest
