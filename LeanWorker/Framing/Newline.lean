module

public import LeanWorker.Framing.Parse

public section

namespace LeanWorker
namespace Framing

open Lean
open JsonRpc

private def dropTrailingCR (line : ByteArray) : ByteArray :=
  if line.size > 0 && line.get! (line.size - 1) == (13 : UInt8) then
    line.extract 0 (line.size - 1)
  else
    line

private partial def decodeNewlineAux
    (buffer : ByteArray)
    (index : Nat)
    (start : Nat)
    (acc : Array ByteArray) : Except Error (Array ByteArray × ByteArray) := do
  if index < buffer.size then
    if buffer.get! index == (10 : UInt8) then
      let rawLine := buffer.extract start index
      let line := dropTrailingCR rawLine
      if line.size == 0 then
        decodeNewlineAux buffer (index + 1) (index + 1) acc
      else
        decodeNewlineAux buffer (index + 1) (index + 1) (acc.push line)
    else
      decodeNewlineAux buffer (index + 1) start acc
  else
    let rest := buffer.extract start buffer.size
    return (acc, rest)

def decodeNewlineBytes (buffer : ByteArray) : Except Error (Array ByteArray × ByteArray) :=
  decodeNewlineAux buffer 0 0 #[]

private partial def pullNewlinePayloadAux
    (buffer : ByteArray)
    (index : Nat)
    (start : Nat) : Except Error (Option (ByteArray × ByteArray)) := do
  if index < buffer.size then
    if buffer.get! index == (10 : UInt8) then
      let rawLine := buffer.extract start index
      let line := dropTrailingCR rawLine
      let nextStart := index + 1
      if line.size == 0 then
        pullNewlinePayloadAux buffer nextStart nextStart
      else
        let rest := buffer.extract nextStart buffer.size
        return some (line, rest)
    else
      pullNewlinePayloadAux buffer (index + 1) start
  else
    return none

def pullNewlinePayload? (buffer : ByteArray) : Except Error (Option (ByteArray × ByteArray)) :=
  pullNewlinePayloadAux buffer 0 0

def encodeNewlineBytes (payload : ByteArray) : ByteArray :=
  payload ++ "\n".toUTF8

end Framing
end LeanWorker
