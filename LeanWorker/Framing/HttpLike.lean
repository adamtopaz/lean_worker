module

public import LeanWorker.Framing.Parse

public section

namespace LeanWorker
namespace Framing

open Lean
open JsonRpc

structure HttpLikeConfig where
  startLine : String := "POST / HTTP/1.1"
  headers : List (String × String) := []
deriving Inhabited

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

private partial def decodeHttpLikeAux
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
    let lines := String.splitOn headerText "\r\n"
    let (startLine, headerLines) :=
      match lines with
      | [] => ("", [])
      | line :: rest => (line, rest)
    if startLine.trimAscii.isEmpty then
      throw (framingError "missing start line")
    let headerText := String.intercalate "\r\n" headerLines
    let headers ← parseHeaders headerText
    let contentLength ← parseContentLength headers
    if buffer.size < bodyStart + contentLength then
      return (acc, buffer)
    let bodyBytes := buffer.extract bodyStart (bodyStart + contentLength)
    let rest := buffer.extract (bodyStart + contentLength) buffer.size
    decodeHttpLikeAux rest (acc.push bodyBytes)

def decodeHttpLikeBytes
    (buffer : ByteArray) : Except Error (Array ByteArray × ByteArray) :=
  decodeHttpLikeAux buffer #[]

def encodeHttpLikeBytes (config : HttpLikeConfig) (payload : ByteArray) : ByteArray :=
  let headers := config.headers ++ [("Content-Length", toString payload.size)]
  let headerLines := headers.map fun (key, value) => s!"{key}: {value}"
  let headerText :=
    String.intercalate "\r\n" (config.startLine :: headerLines) ++ "\r\n\r\n"
  headerText.toUTF8 ++ payload

end Framing
end LeanWorker
