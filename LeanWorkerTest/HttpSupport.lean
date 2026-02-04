module

public import LeanWorker
public import LeanWorkerTest.HttpServer
public import LeanWorkerTest.HttpClient
public import Std.Internal.Async.Basic

public section

namespace LeanWorkerTest

open Lean
open LeanWorker
open LeanWorker.JsonRpc
open Std.Internal.IO.Async

def localhost (port : UInt16) : Std.Net.SocketAddress :=
  let addr := Std.Net.IPv4Addr.ofParts 127 0 0 1
  Std.Net.SocketAddress.v4 { addr := addr, port := port }

def portCandidates : List UInt16 :=
  (List.range 20).map fun i => UInt16.ofNat (41000 + i)

def tryServe (port : UInt16) : IO (Option Transport.Tcp.Listener) := do
  try
    let listener ← Async.block <| serve (localhost port)
    return some listener
  catch _ =>
    return none

def withHttpServer (action : UInt16 → IO α) : IO α := do
  let rec loop : List UInt16 → IO α
    | [] => throw <| IO.userError "failed to bind HTTP test server"
    | port :: rest => do
      let listener? ← tryServe port
      match listener? with
      | some listener =>
        try
          action port
        finally
          Async.block listener.shutdown
      | none => loop rest
  loop portCandidates

def withHttpClient (port : UInt16) (action : Client.Client → IO α) : IO α := do
  let client ← connect (localhost port)
  action client

def withRawHttp (port : UInt16) (action : Transport.ByteTransport → IO α) : IO α := do
  let transport ← connectRaw (localhost port)
  action transport

def httpRequest (body : String) (headers : List (String × String) := []) : String :=
  let bodyBytes := body.toUTF8
  let headers := headers ++ [("Content-Length", toString bodyBytes.size)]
  let headerLines := headers.map fun (key, value) => s!"{key}: {value}"
  let headerText := String.intercalate "\r\n" ("POST / HTTP/1.1" :: headerLines)
  headerText ++ "\r\n\r\n" ++ body

def sendRawHttp (transport : Transport.ByteTransport) (request : String) : Async Unit := do
  let bytes := request.toUTF8
  let _ ← LeanWorker.Async.sendOrLog transport.log "http raw send" transport.outbox bytes
  return

partial def recvHttpJson
    (transport : Transport.ByteTransport)
    (buffer : ByteArray := ByteArray.empty) : Async (Except JsonRpc.Error Json) := do
  match Framing.httpLike {} |>.decode buffer with
  | .ok (items, rest) =>
    match items.toList with
    | first :: _ => return .ok first
    | [] =>
      match ← await <| ← transport.inbox.recv with
      | none => return .error JsonRpc.Error.parseError
      | some chunk => recvHttpJson transport (rest ++ chunk)
  | .error err => return .error err

end LeanWorkerTest
