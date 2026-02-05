module

public import LeanWorker.Framing.Types
public import LeanWorker.JsonRpc.Parse

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
    (acc : Array Json) : Except Error (Array Json × ByteArray) := do
  if index < buffer.size then
    if buffer.get! index == (10 : UInt8) then
      let rawLine := buffer.extract start index
      let line := dropTrailingCR rawLine
      if line.size == 0 then
        decodeNewlineAux buffer (index + 1) (index + 1) acc
      else
        match String.fromUTF8? line with
        | none => throw (framingError "invalid UTF-8 in newline frame")
        | some text =>
          let json ← parseJson text
          decodeNewlineAux buffer (index + 1) (index + 1) (acc.push json)
    else
      decodeNewlineAux buffer (index + 1) start acc
  else
    let rest := buffer.extract start buffer.size
    return (acc, rest)

def decodeNewline (buffer : ByteArray) : Except Error (Array Json × ByteArray) :=
  decodeNewlineAux buffer 0 0 #[]

def newline : Framing :=
  {
    encode := fun json => (Json.compress json ++ "\n").toUTF8,
    decode := decodeNewline
  }

end Framing
end LeanWorker
