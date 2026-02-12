module

public import LeanWorker.Encoding.Expr.Binary

public section

namespace LeanWorker
namespace Encoding

open Lean
open Lean.Meta

private def contextWireMagic : ByteArray := ByteArray.mk #[76, 87, 77, 67]
private def stateWireMagic : ByteArray := ByteArray.mk #[76, 87, 77, 83]
private def wireVersion : UInt8 := UInt8.ofNat 1

private abbrev Encoder := StateM ByteArray

private def runEncoder (act : Encoder Unit) : ByteArray :=
  (act.run ByteArray.empty).snd

private def emitByte (byte : UInt8) : Encoder Unit :=
  modify fun out => out.push byte

private def emitBytes (bytes : ByteArray) : Encoder Unit :=
  modify fun out => out ++ bytes

private def emitTag (tag : Nat) : Encoder Unit :=
  emitByte (UInt8.ofNat tag)

private partial def emitVarUInt (value : Nat) : Encoder Unit := do
  let chunk := value % 128
  let rest := value / 128
  if rest == 0 then
    emitByte (UInt8.ofNat chunk)
  else
    emitByte (UInt8.ofNat (chunk + 128))
    emitVarUInt rest

private def encodeBool (value : Bool) : Encoder Unit :=
  emitByte (if value then (1 : UInt8) else (0 : UInt8))

private def encodeByteArray (bytes : ByteArray) : Encoder Unit := do
  emitVarUInt bytes.size
  emitBytes bytes

private def encodeArray {α : Type} (items : Array α) (encodeItem : α → Encoder Unit) : Encoder Unit := do
  emitVarUInt items.size
  for item in items do
    encodeItem item

private def encodeOption {α : Type} (value : Option α) (encodeValue : α → Encoder Unit) : Encoder Unit :=
  match value with
  | none =>
    emitTag 0
  | some v => do
    emitTag 1
    encodeValue v

private partial def encodeName (name : Name) : Encoder Unit :=
  match name with
  | .anonymous =>
    emitTag 0
  | .str pre str => do
    emitTag 1
    encodeName pre
    encodeByteArray str.toUTF8
  | .num pre idx => do
    emitTag 2
    encodeName pre
    emitVarUInt idx

private def encodeFVarId (fvarId : FVarId) : Encoder Unit :=
  encodeName fvarId.name

private def encodeMVarId (mvarId : MVarId) : Encoder Unit :=
  encodeName mvarId.name

private def encodeLMVarId (mvarId : LMVarId) : Encoder Unit :=
  encodeName mvarId.name

private def encodeBinderInfo (binderInfo : BinderInfo) : Encoder Unit :=
  match binderInfo with
  | .default => emitTag 0
  | .implicit => emitTag 1
  | .strictImplicit => emitTag 2
  | .instImplicit => emitTag 3

private def encodeLocalDeclKind (kind : LocalDeclKind) : Encoder Unit :=
  match kind with
  | .default => emitTag 0
  | .implDetail => emitTag 1
  | .auxDecl => emitTag 2

private def encodeMetavarKind (kind : MetavarKind) : Encoder Unit :=
  match kind with
  | .natural => emitTag 0
  | .synthetic => emitTag 1
  | .syntheticOpaque => emitTag 2

private def encodeTransparencyMode (mode : TransparencyMode) : Encoder Unit :=
  match mode with
  | .all => emitTag 0
  | .default => emitTag 1
  | .reducible => emitTag 2
  | .instances => emitTag 3

private def encodeEtaStructMode (mode : EtaStructMode) : Encoder Unit :=
  match mode with
  | .all => emitTag 0
  | .notClasses => emitTag 1
  | .none => emitTag 2

private def encodeProjReductionKind (kind : ProjReductionKind) : Encoder Unit :=
  match kind with
  | .no => emitTag 0
  | .yes => emitTag 1
  | .yesWithDelta => emitTag 2
  | .yesWithDeltaI => emitTag 3

private def encodeExprField (expr : Expr) : Encoder Unit :=
  encodeByteArray (encodeExprRaw expr)

private def encodeLevelField (level : Level) : Encoder Unit :=
  encodeExprField (.sort level)

private def encodeMetaConfig (cfg : Meta.Config) : Encoder Unit := do
  encodeBool cfg.foApprox
  encodeBool cfg.ctxApprox
  encodeBool cfg.quasiPatternApprox
  encodeBool cfg.constApprox
  encodeBool cfg.isDefEqStuckEx
  encodeBool cfg.unificationHints
  encodeBool cfg.proofIrrelevance
  encodeBool cfg.assignSyntheticOpaque
  encodeBool cfg.offsetCnstrs
  encodeTransparencyMode cfg.transparency
  encodeEtaStructMode cfg.etaStruct
  encodeBool cfg.univApprox
  encodeBool cfg.iota
  encodeBool cfg.beta
  encodeProjReductionKind cfg.proj
  encodeBool cfg.zeta
  encodeBool cfg.zetaDelta
  encodeBool cfg.zetaUnused
  encodeBool cfg.zetaHave

private def encodeLocalInstance (inst : LocalInstance) : Encoder Unit := do
  encodeName inst.className
  encodeExprField inst.fvar

private def encodeLocalInstances (insts : LocalInstances) : Encoder Unit :=
  encodeArray insts encodeLocalInstance

private def fvarIdSetToArray (set : FVarIdSet) : Array FVarId := Id.run do
  let mut out := #[]
  for fvarId in set do
    out := out.push fvarId
  return out

private def encodeFVarIdSet (set : FVarIdSet) : Encoder Unit :=
  encodeArray (fvarIdSetToArray set) encodeFVarId

private def encodeLocalDecl (decl : LocalDecl) : Encoder Unit :=
  match decl with
  | .cdecl index fvarId userName type binderInfo kind => do
    emitTag 0
    emitVarUInt index
    encodeFVarId fvarId
    encodeName userName
    encodeExprField type
    encodeBinderInfo binderInfo
    encodeLocalDeclKind kind
  | .ldecl index fvarId userName type value nondep kind => do
    emitTag 1
    emitVarUInt index
    encodeFVarId fvarId
    encodeName userName
    encodeExprField type
    encodeExprField value
    encodeBool nondep
    encodeLocalDeclKind kind

private def localContextDeclSlots (lctx : LocalContext) : Array (Option LocalDecl) := Id.run do
  let mut out := Array.emptyWithCapacity lctx.numIndices
  for i in [0:lctx.numIndices] do
    out := out.push (lctx.getAt? i)
  return out

private def localContextAuxEntries (lctx : LocalContext) : Array (FVarId × Name) := Id.run do
  lctx.auxDeclToFullName.foldl
    (fun out fvarId fullName => out.push (fvarId, fullName))
    #[]

private def encodeLocalContext (lctx : LocalContext) : Encoder Unit := do
  encodeArray (localContextDeclSlots lctx) fun decl? =>
    encodeOption decl? encodeLocalDecl
  encodeArray (localContextAuxEntries lctx) fun entry => do
    encodeFVarId entry.fst
    encodeName entry.snd

private def encodeDefEqContext (ctx : DefEqContext) : Encoder Unit := do
  encodeExprField ctx.lhs
  encodeExprField ctx.rhs
  encodeLocalContext ctx.lctx
  encodeLocalInstances ctx.localInstances

private def encodeMetavarDecl (decl : MetavarDecl) : Encoder Unit := do
  encodeName decl.userName
  encodeLocalContext decl.lctx
  encodeExprField decl.type
  emitVarUInt decl.depth
  encodeLocalInstances decl.localInstances
  encodeMetavarKind decl.kind
  emitVarUInt decl.numScopeArgs
  emitVarUInt decl.index

private def encodeDelayedMetavarAssignment (assignment : DelayedMetavarAssignment) : Encoder Unit := do
  encodeArray assignment.fvars encodeExprField
  encodeMVarId assignment.mvarIdPending

private def hashMapEntries {α : Type} {β : Type} [BEq α] [Hashable α]
    (map : PersistentHashMap α β) : Array (α × β) := Id.run do
  let mut out := #[]
  for entry in map do
    out := out.push entry
  return out

private def encodeHashMap {α : Type} {β : Type} [BEq α] [Hashable α]
    (map : PersistentHashMap α β)
    (encodeKey : α → Encoder Unit)
    (encodeValue : β → Encoder Unit)
    : Encoder Unit :=
  encodeArray (hashMapEntries map) fun entry => do
    encodeKey entry.fst
    encodeValue entry.snd

private def encodeMetavarContext (mctx : MetavarContext) : Encoder Unit := do
  emitVarUInt mctx.depth
  emitVarUInt mctx.levelAssignDepth
  emitVarUInt mctx.mvarCounter
  encodeHashMap mctx.lDepth encodeLMVarId emitVarUInt
  encodeHashMap mctx.decls encodeMVarId encodeMetavarDecl
  encodeHashMap mctx.userNames encodeName encodeMVarId
  encodeHashMap mctx.lAssignment encodeLMVarId encodeLevelField
  encodeHashMap mctx.eAssignment encodeMVarId encodeExprField
  encodeHashMap mctx.dAssignment encodeMVarId encodeDelayedMetavarAssignment

private def encodePostponedEntry (entry : PostponedEntry) : Encoder Unit := do
  encodeLevelField entry.lhs
  encodeLevelField entry.rhs
  encodeOption entry.ctx? encodeDefEqContext

private def encodeMetaContextPayload? (ctx : Meta.Context) : Except String ByteArray := do
  if ctx.canUnfold?.isSome then
    throw "cannot encode Meta.Context when `canUnfold?` is set"
  return runEncoder do
    encodeMetaConfig ctx.keyedConfig.config
    encodeBool ctx.trackZetaDelta
    encodeFVarIdSet ctx.zetaDeltaSet
    encodeLocalContext ctx.lctx
    encodeLocalInstances ctx.localInstances
    encodeOption ctx.defEqCtx? encodeDefEqContext
    emitVarUInt ctx.synthPendingDepth
    encodeBool ctx.univApprox
    encodeBool ctx.inTypeClassResolution

private def encodeMetaStatePayload (state : Meta.State) : ByteArray :=
  runEncoder do
    encodeMetavarContext state.mctx
    encodeFVarIdSet state.zetaDeltaFVarIds
    encodeArray state.postponed.toArray encodePostponedEntry

private def encodeWirePayload (magic payload : ByteArray) : ByteArray :=
  magic ++ (ByteArray.mk #[wireVersion]) ++ payload

private def readByteAt (input : ByteArray) (index : Nat) : Except String (UInt8 × Nat) := do
  if h : index < input.size then
    return (input.get index h, index + 1)
  else
    throw s!"unexpected end of input at byte index {index}"

private def readTagAt (input : ByteArray) (index : Nat) : Except String (Nat × Nat) := do
  let (byte, nextIndex) ← readByteAt input index
  return (byte.toNat, nextIndex)

private def readSliceAt (input : ByteArray) (index length : Nat) : Except String (ByteArray × Nat) := do
  let stop := index + length
  if stop ≤ input.size then
    return (input.extract index stop, stop)
  else
    throw s!"unexpected end of input while reading {length} bytes at byte index {index}"

private partial def readVarUIntLoop (input : ByteArray) (index factor acc : Nat) : Except String (Nat × Nat) := do
  let (byte, nextIndex) ← readByteAt input index
  let payload := byte.toNat % 128
  let acc := acc + payload * factor
  if byte.toNat < 128 then
    return (acc, nextIndex)
  else
    readVarUIntLoop input nextIndex (factor * 128) acc

private def readVarUIntAt (input : ByteArray) (index : Nat) : Except String (Nat × Nat) :=
  readVarUIntLoop input index 1 0

private def decodeBoolAt (input : ByteArray) (index : Nat) : Except String (Bool × Nat) := do
  let (byte, nextIndex) ← readByteAt input index
  match byte.toNat with
  | 0 => return (false, nextIndex)
  | 1 => return (true, nextIndex)
  | tag => throw s!"invalid bool tag {tag} at byte index {index}"

private def decodeByteArrayAt (input : ByteArray) (index : Nat) : Except String (ByteArray × Nat) := do
  let (length, index) ← readVarUIntAt input index
  readSliceAt input index length

private def decodeArrayAt {α : Type}
    (input : ByteArray)
    (index : Nat)
    (decodeItem : ByteArray → Nat → Except String (α × Nat))
    : Except String (Array α × Nat) := do
  let (size, index) ← readVarUIntAt input index
  let mut out := Array.emptyWithCapacity size
  let mut cursor := index
  for _ in [0:size] do
    let (item, nextCursor) ← decodeItem input cursor
    out := out.push item
    cursor := nextCursor
  return (out, cursor)

private def decodeOptionAt {α : Type}
    (input : ByteArray)
    (index : Nat)
    (decodeValue : ByteArray → Nat → Except String (α × Nat))
    : Except String (Option α × Nat) := do
  let (tag, index) ← readTagAt input index
  match tag with
  | 0 =>
    return (none, index)
  | 1 => do
    let (value, nextIndex) ← decodeValue input index
    return (some value, nextIndex)
  | _ =>
    throw s!"invalid option tag {tag} at byte index {index - 1}"

private partial def decodeNameAt (input : ByteArray) (index : Nat) : Except String (Name × Nat) := do
  let (tag, index) ← readTagAt input index
  match tag with
  | 0 =>
    return (.anonymous, index)
  | 1 => do
    let (pre, index) ← decodeNameAt input index
    let (strBytes, index) ← decodeByteArrayAt input index
    match String.fromUTF8? strBytes with
    | some str =>
      return (.str pre str, index)
    | none =>
      throw s!"invalid UTF-8 string payload at byte index {index - strBytes.size}"
  | 2 => do
    let (pre, index) ← decodeNameAt input index
    let (idx, index) ← readVarUIntAt input index
    return (.num pre idx, index)
  | _ =>
    throw s!"invalid name tag {tag} at byte index {index - 1}"

private def decodeFVarIdAt (input : ByteArray) (index : Nat) : Except String (FVarId × Nat) := do
  let (name, nextIndex) ← decodeNameAt input index
  return (⟨name⟩, nextIndex)

private def decodeMVarIdAt (input : ByteArray) (index : Nat) : Except String (MVarId × Nat) := do
  let (name, nextIndex) ← decodeNameAt input index
  return (⟨name⟩, nextIndex)

private def decodeLMVarIdAt (input : ByteArray) (index : Nat) : Except String (LMVarId × Nat) := do
  let (name, nextIndex) ← decodeNameAt input index
  return (⟨name⟩, nextIndex)

private def decodeBinderInfoAt (input : ByteArray) (index : Nat) : Except String (BinderInfo × Nat) := do
  let (tag, nextIndex) ← readTagAt input index
  match tag with
  | 0 => return (.default, nextIndex)
  | 1 => return (.implicit, nextIndex)
  | 2 => return (.strictImplicit, nextIndex)
  | 3 => return (.instImplicit, nextIndex)
  | _ => throw s!"invalid binder info tag {tag} at byte index {index}"

private def decodeLocalDeclKindAt (input : ByteArray) (index : Nat) : Except String (LocalDeclKind × Nat) := do
  let (tag, nextIndex) ← readTagAt input index
  match tag with
  | 0 => return (.default, nextIndex)
  | 1 => return (.implDetail, nextIndex)
  | 2 => return (.auxDecl, nextIndex)
  | _ => throw s!"invalid local decl kind tag {tag} at byte index {index}"

private def decodeMetavarKindAt (input : ByteArray) (index : Nat) : Except String (MetavarKind × Nat) := do
  let (tag, nextIndex) ← readTagAt input index
  match tag with
  | 0 => return (.natural, nextIndex)
  | 1 => return (.synthetic, nextIndex)
  | 2 => return (.syntheticOpaque, nextIndex)
  | _ => throw s!"invalid metavariable kind tag {tag} at byte index {index}"

private def decodeTransparencyModeAt (input : ByteArray) (index : Nat) : Except String (TransparencyMode × Nat) := do
  let (tag, nextIndex) ← readTagAt input index
  match tag with
  | 0 => return (.all, nextIndex)
  | 1 => return (.default, nextIndex)
  | 2 => return (.reducible, nextIndex)
  | 3 => return (.instances, nextIndex)
  | _ => throw s!"invalid transparency mode tag {tag} at byte index {index}"

private def decodeEtaStructModeAt (input : ByteArray) (index : Nat) : Except String (EtaStructMode × Nat) := do
  let (tag, nextIndex) ← readTagAt input index
  match tag with
  | 0 => return (.all, nextIndex)
  | 1 => return (.notClasses, nextIndex)
  | 2 => return (.none, nextIndex)
  | _ => throw s!"invalid eta-struct mode tag {tag} at byte index {index}"

private def decodeProjReductionKindAt (input : ByteArray) (index : Nat) : Except String (ProjReductionKind × Nat) := do
  let (tag, nextIndex) ← readTagAt input index
  match tag with
  | 0 => return (.no, nextIndex)
  | 1 => return (.yes, nextIndex)
  | 2 => return (.yesWithDelta, nextIndex)
  | 3 => return (.yesWithDeltaI, nextIndex)
  | _ => throw s!"invalid projection-reduction kind tag {tag} at byte index {index}"

private def decodeExprFieldAt (input : ByteArray) (index : Nat) : Except String (Expr × Nat) := do
  let (bytes, nextIndex) ← decodeByteArrayAt input index
  match decodeExprRaw? bytes with
  | .ok expr =>
    return (expr, nextIndex)
  | .error err =>
    throw s!"invalid encoded expression: {err}"

private def decodeLevelFieldAt (input : ByteArray) (index : Nat) : Except String (Level × Nat) := do
  let (expr, nextIndex) ← decodeExprFieldAt input index
  match expr with
  | .sort level =>
    return (level, nextIndex)
  | _ =>
    throw s!"invalid encoded level expression at byte index {index}"

private def decodeMetaConfigAt (input : ByteArray) (index : Nat) : Except String (Meta.Config × Nat) := do
  let (foApprox, index) ← decodeBoolAt input index
  let (ctxApprox, index) ← decodeBoolAt input index
  let (quasiPatternApprox, index) ← decodeBoolAt input index
  let (constApprox, index) ← decodeBoolAt input index
  let (isDefEqStuckEx, index) ← decodeBoolAt input index
  let (unificationHints, index) ← decodeBoolAt input index
  let (proofIrrelevance, index) ← decodeBoolAt input index
  let (assignSyntheticOpaque, index) ← decodeBoolAt input index
  let (offsetCnstrs, index) ← decodeBoolAt input index
  let (transparency, index) ← decodeTransparencyModeAt input index
  let (etaStruct, index) ← decodeEtaStructModeAt input index
  let (univApprox, index) ← decodeBoolAt input index
  let (iota, index) ← decodeBoolAt input index
  let (beta, index) ← decodeBoolAt input index
  let (proj, index) ← decodeProjReductionKindAt input index
  let (zeta, index) ← decodeBoolAt input index
  let (zetaDelta, index) ← decodeBoolAt input index
  let (zetaUnused, index) ← decodeBoolAt input index
  let (zetaHave, nextIndex) ← decodeBoolAt input index
  return ({
    foApprox,
    ctxApprox,
    quasiPatternApprox,
    constApprox,
    isDefEqStuckEx,
    unificationHints,
    proofIrrelevance,
    assignSyntheticOpaque,
    offsetCnstrs,
    transparency,
    etaStruct,
    univApprox,
    iota,
    beta,
    proj,
    zeta,
    zetaDelta,
    zetaUnused,
    zetaHave
  }, nextIndex)

private def decodeLocalInstanceAt (input : ByteArray) (index : Nat) : Except String (LocalInstance × Nat) := do
  let (className, index) ← decodeNameAt input index
  let (fvar, nextIndex) ← decodeExprFieldAt input index
  return ({ className, fvar }, nextIndex)

private def decodeLocalInstancesAt (input : ByteArray) (index : Nat) : Except String (LocalInstances × Nat) :=
  decodeArrayAt input index decodeLocalInstanceAt

private def decodeFVarIdSetAt (input : ByteArray) (index : Nat) : Except String (FVarIdSet × Nat) := do
  let (items, nextIndex) ← decodeArrayAt input index decodeFVarIdAt
  let set := items.foldl (init := ({} : FVarIdSet)) fun acc fvarId => acc.insert fvarId
  return (set, nextIndex)

private def decodeLocalDeclAt (input : ByteArray) (index : Nat) : Except String (LocalDecl × Nat) := do
  let (tag, index) ← readTagAt input index
  match tag with
  | 0 => do
    let (declIndex, index) ← readVarUIntAt input index
    let (fvarId, index) ← decodeFVarIdAt input index
    let (userName, index) ← decodeNameAt input index
    let (type, index) ← decodeExprFieldAt input index
    let (binderInfo, index) ← decodeBinderInfoAt input index
    let (kind, nextIndex) ← decodeLocalDeclKindAt input index
    return (.cdecl declIndex fvarId userName type binderInfo kind, nextIndex)
  | 1 => do
    let (declIndex, index) ← readVarUIntAt input index
    let (fvarId, index) ← decodeFVarIdAt input index
    let (userName, index) ← decodeNameAt input index
    let (type, index) ← decodeExprFieldAt input index
    let (value, index) ← decodeExprFieldAt input index
    let (nondep, index) ← decodeBoolAt input index
    let (kind, nextIndex) ← decodeLocalDeclKindAt input index
    return (.ldecl declIndex fvarId userName type value nondep kind, nextIndex)
  | _ =>
    throw s!"invalid local declaration tag {tag} at byte index {index - 1}"

private def decodeLocalContextAt (input : ByteArray) (index : Nat) : Except String (LocalContext × Nat) := do
  let (declSlots, index) ← decodeArrayAt input index fun bytes cursor =>
    decodeOptionAt bytes cursor decodeLocalDeclAt
  let (auxEntries, nextIndex) ← decodeArrayAt input index fun bytes cursor => do
    let (fvarId, cursor) ← decodeFVarIdAt bytes cursor
    let (fullName, cursor) ← decodeNameAt bytes cursor
    return ((fvarId, fullName), cursor)

  let mut decls : PersistentArray (Option LocalDecl) := {}
  let mut fvarIdToDecl : PersistentHashMap FVarId LocalDecl := {}
  for i in [0:declSlots.size] do
    let decl? := declSlots[i]!
    decls := decls.push decl?
    match decl? with
    | none =>
      pure ()
    | some decl =>
      if decl.index != i then
        throw s!"local declaration index mismatch at slot {i}: got {decl.index}"
      if (fvarIdToDecl.find? decl.fvarId).isSome then
        throw s!"duplicate local declaration for free variable `{decl.fvarId.name}`"
      fvarIdToDecl := fvarIdToDecl.insert decl.fvarId decl

  let mut auxDeclToFullName : FVarIdMap Name := {}
  let mut seenAux : PersistentHashMap FVarId PUnit := {}
  for entry in auxEntries do
    let fvarId := entry.fst
    let fullName := entry.snd
    if seenAux.contains fvarId then
      throw s!"duplicate auxiliary-declaration name entry for `{fvarId.name}`"
    seenAux := seenAux.insert fvarId ⟨⟩
    let some decl := fvarIdToDecl.find? fvarId
      | throw s!"auxiliary-declaration name entry references unknown free variable `{fvarId.name}`"
    unless decl.isAuxDecl do
      throw s!"auxiliary-declaration name entry for `{fvarId.name}` does not reference an aux declaration"
    auxDeclToFullName := auxDeclToFullName.insert fvarId fullName

  return ({ fvarIdToDecl, decls, auxDeclToFullName }, nextIndex)

private def decodeDefEqContextAt (input : ByteArray) (index : Nat) : Except String (DefEqContext × Nat) := do
  let (lhs, index) ← decodeExprFieldAt input index
  let (rhs, index) ← decodeExprFieldAt input index
  let (lctx, index) ← decodeLocalContextAt input index
  let (localInstances, nextIndex) ← decodeLocalInstancesAt input index
  return ({ lhs, rhs, lctx, localInstances }, nextIndex)

private def decodeMetavarDeclAt (input : ByteArray) (index : Nat) : Except String (MetavarDecl × Nat) := do
  let (userName, index) ← decodeNameAt input index
  let (lctx, index) ← decodeLocalContextAt input index
  let (type, index) ← decodeExprFieldAt input index
  let (depth, index) ← readVarUIntAt input index
  let (localInstances, index) ← decodeLocalInstancesAt input index
  let (kind, index) ← decodeMetavarKindAt input index
  let (numScopeArgs, index) ← readVarUIntAt input index
  let (declIndex, nextIndex) ← readVarUIntAt input index
  return ({
    userName,
    lctx,
    type,
    depth,
    localInstances,
    kind,
    numScopeArgs,
    index := declIndex
  }, nextIndex)

private def decodeDelayedMetavarAssignmentAt
    (input : ByteArray)
    (index : Nat)
    : Except String (DelayedMetavarAssignment × Nat) := do
  let (fvars, index) ← decodeArrayAt input index decodeExprFieldAt
  let (mvarIdPending, nextIndex) ← decodeMVarIdAt input index
  return ({ fvars, mvarIdPending }, nextIndex)

private def decodeHashMapAt {α : Type} {β : Type} [BEq α] [Hashable α]
    (input : ByteArray)
    (index : Nat)
    (decodeKey : ByteArray → Nat → Except String (α × Nat))
    (decodeValue : ByteArray → Nat → Except String (β × Nat))
    : Except String (PersistentHashMap α β × Nat) := do
  let (entries, nextIndex) ← decodeArrayAt input index fun bytes cursor => do
    let (key, cursor) ← decodeKey bytes cursor
    let (value, cursor) ← decodeValue bytes cursor
    return ((key, value), cursor)
  let map := entries.foldl (init := ({} : PersistentHashMap α β)) fun acc entry =>
    acc.insert entry.fst entry.snd
  return (map, nextIndex)

private def decodeMetavarContextAt (input : ByteArray) (index : Nat) : Except String (MetavarContext × Nat) := do
  let (depth, index) ← readVarUIntAt input index
  let (levelAssignDepth, index) ← readVarUIntAt input index
  let (mvarCounter, index) ← readVarUIntAt input index
  let (lDepth, index) ← decodeHashMapAt input index decodeLMVarIdAt readVarUIntAt
  let (decls, index) ← decodeHashMapAt input index decodeMVarIdAt decodeMetavarDeclAt
  let (userNames, index) ← decodeHashMapAt input index decodeNameAt decodeMVarIdAt
  let (lAssignment, index) ← decodeHashMapAt input index decodeLMVarIdAt decodeLevelFieldAt
  let (eAssignment, index) ← decodeHashMapAt input index decodeMVarIdAt decodeExprFieldAt
  let (dAssignment, nextIndex) ← decodeHashMapAt input index decodeMVarIdAt decodeDelayedMetavarAssignmentAt
  return ({
    depth,
    levelAssignDepth,
    mvarCounter,
    lDepth,
    decls,
    userNames,
    lAssignment,
    eAssignment,
    dAssignment
  }, nextIndex)

private def decodePostponedEntryAt (input : ByteArray) (index : Nat) : Except String (PostponedEntry × Nat) := do
  let (lhs, index) ← decodeLevelFieldAt input index
  let (rhs, index) ← decodeLevelFieldAt input index
  let (ctx?, nextIndex) ← decodeOptionAt input index decodeDefEqContextAt
  return ({ ref := Syntax.missing, lhs, rhs, ctx? }, nextIndex)

private def decodeMetaContextPayload? (payload : ByteArray) : Except String Meta.Context := do
  let (config, index) ← decodeMetaConfigAt payload 0
  let (trackZetaDelta, index) ← decodeBoolAt payload index
  let (zetaDeltaSet, index) ← decodeFVarIdSetAt payload index
  let (lctx, index) ← decodeLocalContextAt payload index
  let (localInstances, index) ← decodeLocalInstancesAt payload index
  let (defEqCtx?, index) ← decodeOptionAt payload index decodeDefEqContextAt
  let (synthPendingDepth, index) ← readVarUIntAt payload index
  let (univApprox, index) ← decodeBoolAt payload index
  let (inTypeClassResolution, nextIndex) ← decodeBoolAt payload index
  if nextIndex != payload.size then
    throw s!"trailing data after Meta.Context payload at byte index {nextIndex}"
  return {
    keyedConfig := Meta.Config.toConfigWithKey config,
    trackZetaDelta,
    zetaDeltaSet,
    lctx,
    localInstances,
    defEqCtx?,
    synthPendingDepth,
    canUnfold? := none,
    univApprox,
    inTypeClassResolution
  }

private def decodeMetaStatePayload? (payload : ByteArray) : Except String Meta.State := do
  let (mctx, index) ← decodeMetavarContextAt payload 0
  let (zetaDeltaFVarIds, index) ← decodeFVarIdSetAt payload index
  let (postponedItems, nextIndex) ← decodeArrayAt payload index decodePostponedEntryAt
  if nextIndex != payload.size then
    throw s!"trailing data after Meta.State payload at byte index {nextIndex}"
  let postponed := postponedItems.foldl (init := ({} : PersistentArray PostponedEntry)) fun acc entry =>
    acc.push entry
  return {
    mctx,
    zetaDeltaFVarIds,
    postponed,
    cache := {},
    diag := {}
  }

private def decodeWirePayload? (magic : ByteArray) (label : String) (input : ByteArray) : Except String ByteArray := do
  let prefixSize := magic.size + 1
  if input.size < prefixSize then
    throw s!"invalid {label} payload: expected at least {prefixSize} bytes"
  if input.extract 0 magic.size != magic then
    throw s!"invalid {label} payload magic"
  let version := input.get! magic.size
  if version != wireVersion then
    throw s!"unsupported {label} payload version {version.toNat}"
  return input.extract prefixSize input.size

def encodeMetaContext? (ctx : Meta.Context) : CoreM (Except String ByteArray) :=
  return do
    let payload ← encodeMetaContextPayload? ctx
    return encodeWirePayload contextWireMagic payload

def decodeMetaContext? (input : ByteArray) : CoreM (Except String Meta.Context) :=
  return do
    let payload ← decodeWirePayload? contextWireMagic "meta context" input
    decodeMetaContextPayload? payload

def encodeMetaState (state : Meta.State) : CoreM ByteArray :=
  return encodeWirePayload stateWireMagic (encodeMetaStatePayload state)

def decodeMetaState? (input : ByteArray) : CoreM (Except String Meta.State) :=
  return do
    let payload ← decodeWirePayload? stateWireMagic "meta state" input
    decodeMetaStatePayload? payload

end Encoding
end LeanWorker
