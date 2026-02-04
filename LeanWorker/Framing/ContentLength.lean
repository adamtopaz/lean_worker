module

public import LeanWorker.Framing.Types

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
    (acc : Array Json) : Except Error (Array Json × ByteArray) := do
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
    let bodyText ←
      match String.fromUTF8? bodyBytes with
      | some text => Except.ok text
      | none => Except.error (framingError "invalid UTF-8 in body")
    let json ← parseJson bodyText
    decodeContentLengthAux rest (acc.push json)

def decodeContentLength (buffer : ByteArray) : Except Error (Array Json × ByteArray) :=
  decodeContentLengthAux buffer #[]

def encodeContentLength (json : Json) : ByteArray :=
  let body := Json.compress json
  let bodyBytes := body.toUTF8
  let header := s!"Content-Length: {bodyBytes.size}\r\n\r\n"
  header.toUTF8 ++ bodyBytes

def contentLength : Framing :=
  {
    encode := encodeContentLength,
    decode := decodeContentLength
  }

end Framing
end LeanWorker
