module

public import LeanWorker.Framing.Parse

public section

namespace LeanWorker
namespace Framing

open Lean
open JsonRpc

private def findHeaderTerminator (buffer : ByteArray) : Option Nat :=
  let size := buffer.size
  let rec loop (index : Nat) : Option Nat :=
    if index + 3 < size then
      if buffer.get! index == (13 : UInt8)
          && buffer.get! (index + 1) == (10 : UInt8)
          && buffer.get! (index + 2) == (13 : UInt8)
          && buffer.get! (index + 3) == (10 : UInt8) then
        some index
      else
        loop (index + 1)
    else
      none
  loop 0

private partial def decodeContentLengthAux
    (buffer : ByteArray)
    (acc : Array ByteArray) : Except Error (Array ByteArray × ByteArray) := do
  match findHeaderTerminator buffer with
  | none => return (acc, buffer)
  | some headerEnd => do
    let bodyStart := headerEnd + 4
    let headerBytes := buffer.extract 0 headerEnd
    let headerText ←
      match String.fromUTF8? headerBytes with
      | some text => Except.ok text
      | none => Except.error (framingError "invalid UTF-8 in headers")
    let headers ← parseHeaders headerText
    let contentLength ← parseContentLength headers
    if buffer.size < bodyStart + contentLength then
      return (acc, buffer)
    let bodyBytes := buffer.extract bodyStart (bodyStart + contentLength)
    let rest := buffer.extract (bodyStart + contentLength) buffer.size
    decodeContentLengthAux rest (acc.push bodyBytes)

def decodeContentLengthBytes
    (buffer : ByteArray) : Except Error (Array ByteArray × ByteArray) :=
  decodeContentLengthAux buffer #[]

def encodeContentLengthBytes (payload : ByteArray) : ByteArray :=
  let header := s!"Content-Length: {payload.size}\r\n\r\n"
  header.toUTF8 ++ payload

end Framing
end LeanWorker
