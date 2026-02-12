module

public import LeanWorker.Encoding.Binary

public section

namespace LeanWorker
namespace Encoding

open Lean
open Lean.Meta

private def wireMagic : ByteArray := ByteArray.mk #[76, 87, 69, 88]
private def wireVersion : UInt8 := UInt8.ofNat 1
private def wirePrefixSize : Nat := wireMagic.size + 1

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

private def encodeString (text : String) : Encoder Unit := do
  let bytes := text.toUTF8
  emitVarUInt bytes.size
  emitBytes bytes

private def encodeArray {α : Type} (items : Array α) (encodeItem : α → Encoder Unit) : Encoder Unit := do
  emitVarUInt items.size
  for item in items do
    encodeItem item

private def encodeList {α : Type} (items : List α) (encodeItem : α → Encoder Unit) : Encoder Unit :=
  encodeArray items.toArray encodeItem

private def encodePosRaw (pos : String.Pos.Raw) : Encoder Unit :=
  emitVarUInt pos.byteIdx

private def encodeSubstringRaw (sub : Substring.Raw) : Encoder Unit := do
  encodeString sub.str
  encodePosRaw sub.startPos
  encodePosRaw sub.stopPos

private partial def encodeName (name : Name) : Encoder Unit :=
  match name with
  | .anonymous =>
    emitTag 0
  | .str pre str => do
    emitTag 1
    encodeName pre
    encodeString str
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

private def encodeInt (value : Int) : Encoder Unit :=
  match value with
  | .ofNat n => do
    emitTag 0
    emitVarUInt n
  | .negSucc n => do
    emitTag 1
    emitVarUInt n

private def encodeBinderInfo (binderInfo : BinderInfo) : Encoder Unit :=
  match binderInfo with
  | .default => emitTag 0
  | .implicit => emitTag 1
  | .strictImplicit => emitTag 2
  | .instImplicit => emitTag 3

private def encodeLiteral (lit : Literal) : Encoder Unit :=
  match lit with
  | .natVal val => do
    emitTag 0
    emitVarUInt val
  | .strVal val => do
    emitTag 1
    encodeString val

private partial def encodeLevel (level : Level) : Encoder Unit :=
  match level with
  | .zero =>
    emitTag 0
  | .succ u => do
    emitTag 1
    encodeLevel u
  | .max u v => do
    emitTag 2
    encodeLevel u
    encodeLevel v
  | .imax u v => do
    emitTag 3
    encodeLevel u
    encodeLevel v
  | .param name => do
    emitTag 4
    encodeName name
  | .mvar mvarId => do
    emitTag 5
    encodeLMVarId mvarId

private def encodeSourceInfo (info : SourceInfo) : Encoder Unit :=
  match info with
  | .none =>
    emitTag 0
  | .original leading pos trailing endPos => do
    emitTag 1
    encodeSubstringRaw leading
    encodePosRaw pos
    encodeSubstringRaw trailing
    encodePosRaw endPos
  | .synthetic pos endPos canonical => do
    emitTag 2
    encodePosRaw pos
    encodePosRaw endPos
    encodeBool canonical

private def encodePreresolved (preresolved : Syntax.Preresolved) : Encoder Unit :=
  match preresolved with
  | .namespace ns => do
    emitTag 0
    encodeName ns
  | .decl declName fields => do
    emitTag 1
    encodeName declName
    encodeList fields encodeString

private partial def encodeSyntax (stx : Syntax) : Encoder Unit :=
  match stx with
  | .missing =>
    emitTag 0
  | .node info kind args => do
    emitTag 1
    encodeSourceInfo info
    encodeName kind
    encodeArray args encodeSyntax
  | .atom info val => do
    emitTag 2
    encodeSourceInfo info
    encodeString val
  | .ident info rawVal val preresolved => do
    emitTag 3
    encodeSourceInfo info
    encodeSubstringRaw rawVal
    encodeName val
    encodeList preresolved encodePreresolved

private partial def encodeDataValue (value : DataValue) : Encoder Unit :=
  match value with
  | .ofString str => do
    emitTag 0
    encodeString str
  | .ofBool b => do
    emitTag 1
    encodeBool b
  | .ofName name => do
    emitTag 2
    encodeName name
  | .ofNat n => do
    emitTag 3
    emitVarUInt n
  | .ofInt i => do
    emitTag 4
    encodeInt i
  | .ofSyntax stx => do
    emitTag 5
    encodeSyntax stx

private def encodeKVMap (map : KVMap) : Encoder Unit :=
  encodeList map.entries fun (entry : Name × DataValue) => do
    encodeName entry.fst
    encodeDataValue entry.snd

private partial def encodeExprCore (expr : Expr) : Encoder Unit :=
  match expr with
  | .bvar idx => do
    emitTag 0
    emitVarUInt idx
  | .fvar fvarId => do
    emitTag 1
    encodeFVarId fvarId
  | .mvar mvarId => do
    emitTag 2
    encodeMVarId mvarId
  | .sort level => do
    emitTag 3
    encodeLevel level
  | .const declName us => do
    emitTag 4
    encodeName declName
    encodeList us encodeLevel
  | .app fn arg => do
    emitTag 5
    encodeExprCore fn
    encodeExprCore arg
  | .lam binderName binderType body binderInfo => do
    emitTag 6
    encodeName binderName
    encodeExprCore binderType
    encodeExprCore body
    encodeBinderInfo binderInfo
  | .forallE binderName binderType body binderInfo => do
    emitTag 7
    encodeName binderName
    encodeExprCore binderType
    encodeExprCore body
    encodeBinderInfo binderInfo
  | .letE declName type value body nondep => do
    emitTag 8
    encodeName declName
    encodeExprCore type
    encodeExprCore value
    encodeExprCore body
    encodeBool nondep
  | .lit lit => do
    emitTag 9
    encodeLiteral lit
  | .mdata data inner => do
    emitTag 10
    encodeKVMap data
    encodeExprCore inner
  | .proj typeName idx structExpr => do
    emitTag 11
    encodeName typeName
    emitVarUInt idx
    encodeExprCore structExpr

private def encodeWirePayload (payload : ByteArray) : ByteArray :=
  wireMagic ++ (ByteArray.mk #[wireVersion]) ++ payload

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

private def decodeStringAt (input : ByteArray) (index : Nat) : Except String (String × Nat) := do
  let (length, index) ← readVarUIntAt input index
  let (bytes, nextIndex) ← readSliceAt input index length
  match String.fromUTF8? bytes with
  | some text => return (text, nextIndex)
  | none => throw s!"invalid UTF-8 string payload at byte index {index}"

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

private def decodeListAt {α : Type}
    (input : ByteArray)
    (index : Nat)
    (decodeItem : ByteArray → Nat → Except String (α × Nat))
    : Except String (List α × Nat) := do
  let (items, nextIndex) ← decodeArrayAt input index decodeItem
  return (items.toList, nextIndex)

private def decodePosRawAt (input : ByteArray) (index : Nat) : Except String (String.Pos.Raw × Nat) := do
  let (byteIdx, nextIndex) ← readVarUIntAt input index
  return ({ byteIdx }, nextIndex)

private def decodeSubstringRawAt (input : ByteArray) (index : Nat) : Except String (Substring.Raw × Nat) := do
  let (str, index) ← decodeStringAt input index
  let (startPos, index) ← decodePosRawAt input index
  let (stopPos, index) ← decodePosRawAt input index
  return ({ str, startPos, stopPos }, index)

private partial def decodeNameAt (input : ByteArray) (index : Nat) : Except String (Name × Nat) := do
  let (tag, index) ← readTagAt input index
  match tag with
  | 0 =>
    return (.anonymous, index)
  | 1 => do
    let (pre, index) ← decodeNameAt input index
    let (str, index) ← decodeStringAt input index
    return (.str pre str, index)
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

private def decodeIntAt (input : ByteArray) (index : Nat) : Except String (Int × Nat) := do
  let (tag, index) ← readTagAt input index
  match tag with
  | 0 =>
    let (value, nextIndex) ← readVarUIntAt input index
    return (.ofNat value, nextIndex)
  | 1 =>
    let (value, nextIndex) ← readVarUIntAt input index
    return (.negSucc value, nextIndex)
  | _ =>
    throw s!"invalid int tag {tag} at byte index {index - 1}"

private def decodeBinderInfoAt (input : ByteArray) (index : Nat) : Except String (BinderInfo × Nat) := do
  let (tag, nextIndex) ← readTagAt input index
  match tag with
  | 0 => return (.default, nextIndex)
  | 1 => return (.implicit, nextIndex)
  | 2 => return (.strictImplicit, nextIndex)
  | 3 => return (.instImplicit, nextIndex)
  | _ => throw s!"invalid binder info tag {tag} at byte index {index}"

private def decodeLiteralAt (input : ByteArray) (index : Nat) : Except String (Literal × Nat) := do
  let (tag, index) ← readTagAt input index
  match tag with
  | 0 =>
    let (value, nextIndex) ← readVarUIntAt input index
    return (.natVal value, nextIndex)
  | 1 =>
    let (value, nextIndex) ← decodeStringAt input index
    return (.strVal value, nextIndex)
  | _ =>
    throw s!"invalid literal tag {tag} at byte index {index - 1}"

private partial def decodeLevelAt (input : ByteArray) (index : Nat) : Except String (Level × Nat) := do
  let (tag, index) ← readTagAt input index
  match tag with
  | 0 =>
    return (.zero, index)
  | 1 => do
    let (u, nextIndex) ← decodeLevelAt input index
    return (.succ u, nextIndex)
  | 2 => do
    let (u, index) ← decodeLevelAt input index
    let (v, nextIndex) ← decodeLevelAt input index
    return (.max u v, nextIndex)
  | 3 => do
    let (u, index) ← decodeLevelAt input index
    let (v, nextIndex) ← decodeLevelAt input index
    return (.imax u v, nextIndex)
  | 4 => do
    let (name, nextIndex) ← decodeNameAt input index
    return (.param name, nextIndex)
  | 5 => do
    let (mvarId, nextIndex) ← decodeLMVarIdAt input index
    return (.mvar mvarId, nextIndex)
  | _ =>
    throw s!"invalid level tag {tag} at byte index {index - 1}"

private def decodeSourceInfoAt (input : ByteArray) (index : Nat) : Except String (SourceInfo × Nat) := do
  let (tag, index) ← readTagAt input index
  match tag with
  | 0 =>
    return (.none, index)
  | 1 => do
    let (leading, index) ← decodeSubstringRawAt input index
    let (pos, index) ← decodePosRawAt input index
    let (trailing, index) ← decodeSubstringRawAt input index
    let (endPos, index) ← decodePosRawAt input index
    return (.original leading pos trailing endPos, index)
  | 2 => do
    let (pos, index) ← decodePosRawAt input index
    let (endPos, index) ← decodePosRawAt input index
    let (canonical, index) ← decodeBoolAt input index
    return (.synthetic pos endPos canonical, index)
  | _ =>
    throw s!"invalid source info tag {tag} at byte index {index - 1}"

private def decodePreresolvedAt (input : ByteArray) (index : Nat) : Except String (Syntax.Preresolved × Nat) := do
  let (tag, index) ← readTagAt input index
  match tag with
  | 0 => do
    let (ns, nextIndex) ← decodeNameAt input index
    return (.namespace ns, nextIndex)
  | 1 => do
    let (declName, index) ← decodeNameAt input index
    let (fields, nextIndex) ← decodeListAt input index decodeStringAt
    return (.decl declName fields, nextIndex)
  | _ =>
    throw s!"invalid preresolved tag {tag} at byte index {index - 1}"

private partial def decodeSyntaxAt (input : ByteArray) (index : Nat) : Except String (Syntax × Nat) := do
  let (tag, index) ← readTagAt input index
  match tag with
  | 0 =>
    return (.missing, index)
  | 1 => do
    let (info, index) ← decodeSourceInfoAt input index
    let (kind, index) ← decodeNameAt input index
    let (args, nextIndex) ← decodeArrayAt input index decodeSyntaxAt
    return (.node info kind args, nextIndex)
  | 2 => do
    let (info, index) ← decodeSourceInfoAt input index
    let (val, nextIndex) ← decodeStringAt input index
    return (.atom info val, nextIndex)
  | 3 => do
    let (info, index) ← decodeSourceInfoAt input index
    let (rawVal, index) ← decodeSubstringRawAt input index
    let (val, index) ← decodeNameAt input index
    let (preresolved, nextIndex) ← decodeListAt input index decodePreresolvedAt
    return (.ident info rawVal val preresolved, nextIndex)
  | _ =>
    throw s!"invalid syntax tag {tag} at byte index {index - 1}"

private partial def decodeDataValueAt (input : ByteArray) (index : Nat) : Except String (DataValue × Nat) := do
  let (tag, index) ← readTagAt input index
  match tag with
  | 0 =>
    let (value, nextIndex) ← decodeStringAt input index
    return (.ofString value, nextIndex)
  | 1 =>
    let (value, nextIndex) ← decodeBoolAt input index
    return (.ofBool value, nextIndex)
  | 2 =>
    let (value, nextIndex) ← decodeNameAt input index
    return (.ofName value, nextIndex)
  | 3 =>
    let (value, nextIndex) ← readVarUIntAt input index
    return (.ofNat value, nextIndex)
  | 4 =>
    let (value, nextIndex) ← decodeIntAt input index
    return (.ofInt value, nextIndex)
  | 5 =>
    let (value, nextIndex) ← decodeSyntaxAt input index
    return (.ofSyntax value, nextIndex)
  | _ =>
    throw s!"invalid data value tag {tag} at byte index {index - 1}"

private def decodeKVMapAt (input : ByteArray) (index : Nat) : Except String (KVMap × Nat) := do
  let decodeEntry := fun (bytes : ByteArray) (cursor : Nat) => do
    let (key, cursor) ← decodeNameAt bytes cursor
    let (value, cursor) ← decodeDataValueAt bytes cursor
    return ((key, value), cursor)
  let (entries, nextIndex) ← decodeListAt input index decodeEntry
  return ({ entries }, nextIndex)

private partial def decodeExprAt (input : ByteArray) (index : Nat) : Except String (Expr × Nat) := do
  let (tag, index) ← readTagAt input index
  match tag with
  | 0 =>
    let (idx, nextIndex) ← readVarUIntAt input index
    return (.bvar idx, nextIndex)
  | 1 =>
    let (fvarId, nextIndex) ← decodeFVarIdAt input index
    return (.fvar fvarId, nextIndex)
  | 2 =>
    let (mvarId, nextIndex) ← decodeMVarIdAt input index
    return (.mvar mvarId, nextIndex)
  | 3 =>
    let (level, nextIndex) ← decodeLevelAt input index
    return (.sort level, nextIndex)
  | 4 => do
    let (declName, index) ← decodeNameAt input index
    let (us, nextIndex) ← decodeListAt input index decodeLevelAt
    return (.const declName us, nextIndex)
  | 5 => do
    let (fn, index) ← decodeExprAt input index
    let (arg, nextIndex) ← decodeExprAt input index
    return (.app fn arg, nextIndex)
  | 6 => do
    let (binderName, index) ← decodeNameAt input index
    let (binderType, index) ← decodeExprAt input index
    let (body, index) ← decodeExprAt input index
    let (binderInfo, nextIndex) ← decodeBinderInfoAt input index
    return (.lam binderName binderType body binderInfo, nextIndex)
  | 7 => do
    let (binderName, index) ← decodeNameAt input index
    let (binderType, index) ← decodeExprAt input index
    let (body, index) ← decodeExprAt input index
    let (binderInfo, nextIndex) ← decodeBinderInfoAt input index
    return (.forallE binderName binderType body binderInfo, nextIndex)
  | 8 => do
    let (declName, index) ← decodeNameAt input index
    let (type, index) ← decodeExprAt input index
    let (value, index) ← decodeExprAt input index
    let (body, index) ← decodeExprAt input index
    let (nondep, nextIndex) ← decodeBoolAt input index
    return (.letE declName type value body nondep, nextIndex)
  | 9 => do
    let (lit, nextIndex) ← decodeLiteralAt input index
    return (.lit lit, nextIndex)
  | 10 => do
    let (data, index) ← decodeKVMapAt input index
    let (inner, nextIndex) ← decodeExprAt input index
    return (.mdata data inner, nextIndex)
  | 11 => do
    let (typeName, index) ← decodeNameAt input index
    let (idx, index) ← readVarUIntAt input index
    let (structExpr, nextIndex) ← decodeExprAt input index
    return (.proj typeName idx structExpr, nextIndex)
  | _ =>
    throw s!"invalid expr tag {tag} at byte index {index - 1}"

private def decodeWirePayload? (input : ByteArray) : Except String ByteArray := do
  if input.size < wirePrefixSize then
    throw s!"invalid expr payload: expected at least {wirePrefixSize} bytes"
  if input.extract 0 wireMagic.size != wireMagic then
    throw "invalid expr payload magic"
  let version := input.get! wireMagic.size
  if version != wireVersion then
    throw s!"unsupported expr payload version {version.toNat}"
  return input.extract wirePrefixSize input.size

private def decodeExprPayload? (payload : ByteArray) : Except String Expr := do
  let (expr, nextIndex) ← decodeExprAt payload 0
  if nextIndex != payload.size then
    throw s!"trailing data after expression payload at byte index {nextIndex}"
  return expr

private abbrev ValidateM := ExceptT String MetaM

private partial def validateLevelRefs (level : Level) : ValidateM Unit :=
  match level with
  | .zero =>
    pure ()
  | .succ u =>
    validateLevelRefs u
  | .max u v => do
    validateLevelRefs u
    validateLevelRefs v
  | .imax u v => do
    validateLevelRefs u
    validateLevelRefs v
  | .param _ =>
    pure ()
  | .mvar mvarId => do
    let mctx ← getMCtx
    if (mctx.findLevelDepth? mvarId).isSome then
      pure ()
    else
      throwThe String s!"unknown universe metavariable `?{mvarId.name}` in decoded expression"

private partial def validateExprRefs (expr : Expr) : ValidateM Unit :=
  match expr with
  | .bvar _ =>
    pure ()
  | .fvar fvarId => do
    let lctx ← getLCtx
    if (lctx.find? fvarId).isSome then
      pure ()
    else
      throwThe String s!"unknown free variable `{fvarId.name}` in decoded expression"
  | .mvar mvarId => do
    let mctx ← getMCtx
    if (mctx.findDecl? mvarId).isSome then
      pure ()
    else
      throwThe String s!"unknown metavariable `?{mvarId.name}` in decoded expression"
  | .sort level =>
    validateLevelRefs level
  | .const _ us =>
    for u in us do
      validateLevelRefs u
  | .app fn arg => do
    validateExprRefs fn
    validateExprRefs arg
  | .lam _ binderType body _ => do
    validateExprRefs binderType
    validateExprRefs body
  | .forallE _ binderType body _ => do
    validateExprRefs binderType
    validateExprRefs body
  | .letE _ type value body _ => do
    validateExprRefs type
    validateExprRefs value
    validateExprRefs body
  | .lit _ =>
    pure ()
  | .mdata _ inner =>
    validateExprRefs inner
  | .proj _ _ structExpr =>
    validateExprRefs structExpr

def encodeExprRaw (expr : Expr) : ByteArray :=
  encodeWirePayload <| runEncoder <| encodeExprCore expr

def decodeExprRaw? (input : ByteArray) : Except String Expr := do
  let payload ← decodeWirePayload? input
  decodeExprPayload? payload

def encodeExpr (expr : Expr) : MetaM ByteArray :=
  return encodeExprRaw expr

def decodeExpr? (input : ByteArray) : MetaM (Except String Expr) := do
  match decodeExprRaw? input with
  | .error err =>
    return .error err
  | .ok expr =>
    match (← validateExprRefs expr |>.run) with
    | .error err =>
      return .error err
    | .ok _ =>
      return .ok expr

end Encoding
end LeanWorker
