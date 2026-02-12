module

public import LeanWorker

public section

namespace LeanWorkerTest

open Lean

def runMetaWithCore (env : Environment) (action : MetaM α) : IO α := do
  let ctx : Core.Context := {
    fileName := "<encoding-tests>",
    fileMap := default
  }
  let st : Core.State := {
    env := env
  }
  (action.run').toIO' ctx st

def runMetaTest (action : MetaM α) : IO α := do
  runMetaWithCore (← mkEmptyEnvironment) action

def runMetaTestWithEnv (env : Environment) (action : MetaM α) : IO α := do
  runMetaWithCore env action

def runCoreWithEnv (env : Environment) (action : CoreM α) : IO α := do
  let ctx : Core.Context := {
    fileName := "<encoding-tests>",
    fileMap := default
  }
  let st : Core.State := {
    env := env
  }
  action.toIO' ctx st

def importInitEnv : IO Environment := do
  let sysroot ← Lean.findSysroot
  Lean.initSearchPath sysroot
  Lean.importModules #[`Init] {}

def getConstInfoOrThrow (env : Environment) (declName : Name) : IO ConstantInfo := do
  match env.find? declName with
  | some info =>
    return info
  | none =>
    throw <| IO.userError s!"missing declaration in imported environment: {declName}"

def getConstValueOrThrow (info : ConstantInfo) : IO Expr := do
  match info.value? with
  | some value =>
    return value
  | none =>
    throw <| IO.userError s!"declaration does not have a value: {info.name}"

def setByteIfPresent (bytes : ByteArray) (index : Nat) (value : UInt8) : ByteArray :=
  if index < bytes.size then
    bytes.set! index value
  else
    bytes

end LeanWorkerTest
