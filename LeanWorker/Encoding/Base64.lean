module

public import Lean

public section

namespace LeanWorker
namespace Encoding

private def padByte : UInt8 := UInt8.ofNat 61

private def inRange (value lo hi : Nat) : Bool :=
  decide (lo ≤ value ∧ value ≤ hi)

private def sextetToBase64Byte (value : Nat) : UInt8 :=
  if value < 26 then
    UInt8.ofNat (65 + value)
  else if value < 52 then
    UInt8.ofNat (97 + (value - 26))
  else if value < 62 then
    UInt8.ofNat (48 + (value - 52))
  else if value == 62 then
    UInt8.ofNat 43
  else
    UInt8.ofNat 47

private def base64ByteToSextet? (byte : UInt8) : Option Nat :=
  let value := byte.toNat
  if inRange value 65 90 then
    some (value - 65)
  else if inRange value 97 122 then
    some (value - 97 + 26)
  else if inRange value 48 57 then
    some (value - 48 + 52)
  else if value == 43 then
    some 62
  else if value == 47 then
    some 63
  else
    none

private def decodeSextet (byte : UInt8) (index : Nat) : Except String Nat := do
  match base64ByteToSextet? byte with
  | some value => return value
  | none => throw s!"invalid base64 character at byte index {index}"

def toBase64Bytes (input : ByteArray) : ByteArray :=
  Id.run do
    let inputSize := input.size
    let fullChunks := inputSize / 3
    let remainder := inputSize % 3
    let outputSize := fullChunks * 4 + if remainder == 0 then 0 else 4
    let mut output := ByteArray.emptyWithCapacity outputSize
    let mut index := 0

    while index + 2 < inputSize do
      let b0 := (input.get! index).toNat
      let b1 := (input.get! (index + 1)).toNat
      let b2 := (input.get! (index + 2)).toNat
      let c0 := b0 / 4
      let c1 := (b0 % 4) * 16 + (b1 / 16)
      let c2 := (b1 % 16) * 4 + (b2 / 64)
      let c3 := b2 % 64
      output := output.push (sextetToBase64Byte c0)
      output := output.push (sextetToBase64Byte c1)
      output := output.push (sextetToBase64Byte c2)
      output := output.push (sextetToBase64Byte c3)
      index := index + 3

    if remainder == 1 then
      let b0 := (input.get! index).toNat
      let c0 := b0 / 4
      let c1 := (b0 % 4) * 16
      output := output.push (sextetToBase64Byte c0)
      output := output.push (sextetToBase64Byte c1)
      output := output.push padByte
      output := output.push padByte
    else if remainder == 2 then
      let b0 := (input.get! index).toNat
      let b1 := (input.get! (index + 1)).toNat
      let c0 := b0 / 4
      let c1 := (b0 % 4) * 16 + (b1 / 16)
      let c2 := (b1 % 16) * 4
      output := output.push (sextetToBase64Byte c0)
      output := output.push (sextetToBase64Byte c1)
      output := output.push (sextetToBase64Byte c2)
      output := output.push padByte

    return output

def fromBase64Bytes? (input : ByteArray) : Except String ByteArray := do
  let inputSize := input.size
  if inputSize % 4 != 0 then
    throw "invalid base64 length: expected a multiple of 4"

  let mut output := ByteArray.emptyWithCapacity ((inputSize / 4) * 3)
  let mut index := 0

  while index < inputSize do
    let c0 := input.get! index
    let c1 := input.get! (index + 1)
    let c2 := input.get! (index + 2)
    let c3 := input.get! (index + 3)
    let isLastChunk := index + 4 == inputSize
    let pad0 := c0 == padByte
    let pad1 := c1 == padByte
    let pad2 := c2 == padByte
    let pad3 := c3 == padByte

    if pad0 || pad1 then
      throw s!"invalid base64 padding at byte index {index}"

    if pad2 then
      if !isLastChunk then
        throw s!"base64 padding is only allowed in the final quartet (byte index {index})"
      if !pad3 then
        throw s!"invalid base64 padding pattern at byte index {index + 2}"
      let s0 ← decodeSextet c0 index
      let s1 ← decodeSextet c1 (index + 1)
      if s1 % 16 != 0 then
        throw s!"non-canonical base64 padding bits at byte index {index + 1}"
      let b0 := UInt8.ofNat (s0 * 4 + s1 / 16)
      output := output.push b0
    else if pad3 then
      if !isLastChunk then
        throw s!"base64 padding is only allowed in the final quartet (byte index {index})"
      let s0 ← decodeSextet c0 index
      let s1 ← decodeSextet c1 (index + 1)
      let s2 ← decodeSextet c2 (index + 2)
      if s2 % 4 != 0 then
        throw s!"non-canonical base64 padding bits at byte index {index + 2}"
      let b0 := UInt8.ofNat (s0 * 4 + s1 / 16)
      let b1 := UInt8.ofNat ((s1 % 16) * 16 + s2 / 4)
      output := output.push b0
      output := output.push b1
    else
      let s0 ← decodeSextet c0 index
      let s1 ← decodeSextet c1 (index + 1)
      let s2 ← decodeSextet c2 (index + 2)
      let s3 ← decodeSextet c3 (index + 3)
      let b0 := UInt8.ofNat (s0 * 4 + s1 / 16)
      let b1 := UInt8.ofNat ((s1 % 16) * 16 + s2 / 4)
      let b2 := UInt8.ofNat ((s2 % 4) * 64 + s3)
      output := output.push b0
      output := output.push b1
      output := output.push b2

    index := index + 4

  return output

def toBase64 (input : ByteArray) : String :=
  String.fromUTF8! (toBase64Bytes input)

def fromBase64? (input : String) : Except String ByteArray :=
  fromBase64Bytes? input.toUTF8

end Encoding
end LeanWorker
