module

public import LeanWorker.JsonRpc.Core

public section

namespace LeanWorker
namespace Framing

open Lean
open JsonRpc

def framingError (message : String) : Error :=
  Error.withData Error.parseError (Json.str message)

def parseHeaderLine (line : String) : Except Error (String × String) :=
  let parts := line.splitOn ":"
  match parts with
  | [] => throw (framingError "header line empty")
  | key :: rest => do
    if rest.isEmpty then
      throw (framingError "header missing ':'")
    else do
      let value := String.intercalate ":" rest
      return (key.trimAscii.toString, value.trimAscii.toString)

def parseHeaders (header : String) : Except Error (List (String × String)) := do
  let lines := header.splitOn "\r\n"
  let lines := lines.filter fun line => !line.trimAscii.isEmpty
  lines.mapM parseHeaderLine

def headerValue? (headers : List (String × String)) (key : String) : Option String :=
  let key := key.trimAscii.toString.toLower
  let header? := headers.find? fun (name, _) => name.trimAscii.toString.toLower == key
  header?.map Prod.snd

def parseContentLength (headers : List (String × String)) : Except Error Nat :=
  match headerValue? headers "content-length" with
  | some value =>
    match value.toNat? with
    | some length => return length
    | none => throw (framingError "invalid Content-Length")
  | none => throw (framingError "missing Content-Length")

end Framing
end LeanWorker
