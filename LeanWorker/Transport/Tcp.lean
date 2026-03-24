module

public import LeanWorker.Transport.Core
public import Std.Internal.Async.TCP
public import Std.Internal.Async.DNS

public section

namespace LeanWorker
namespace Transport

open Std.Net
open Std.Internal.IO.Async

abbrev TcpServerSocket := Std.Internal.IO.Async.TCP.Socket.Server
abbrev TcpClientSocket := Std.Internal.IO.Async.TCP.Socket.Client

structure TcpEndpoint where
  host : String
  port : UInt16
  family? : Option AddressFamily := none

private def byteSourceOfTcpSocket (socket : TcpClientSocket) : ByteSource where
  recv? maxBytes :=
    socket.recv? (UInt64.ofNat maxBytes)

private def byteSinkOfTcpSocket (socket : TcpClientSocket) : ByteSink where
  send bytes :=
    socket.send bytes
  shutdown :=
    socket.shutdown

private def serviceName (port : UInt16) : String :=
  toString port.toNat

def ipAddrToSocketAddress (ipAddr : IPAddr) (port : UInt16) : SocketAddress :=
  match ipAddr with
  | .v4 addr => .v4 { addr, port }
  | .v6 addr => .v6 { addr, port }

-- TODO: move upstream if Lean adds a `ToString SocketAddress` instance.
def socketAddressToString : SocketAddress → String
  | .v4 addr => s!"{addr.addr}:{addr.port.toNat}"
  | .v6 addr => s!"[{addr.addr}]:{addr.port.toNat}"

def transportFromTcpSocket
    (socket : TcpClientSocket)
    (framing : Framing.Spec := .newline)
    (log : LogLevel → String → IO Unit := silentLogger) : Async Transport :=
  transportFromByteStreams
    (byteSourceOfTcpSocket socket)
    (byteSinkOfTcpSocket socket)
    framing
    log

def serverTransportFromAcceptedTcpSocket
    (socket : TcpClientSocket)
    (framing : Framing.Spec := .newline)
    (log : LogLevel → String → IO Unit := silentLogger) : Async Transport :=
  transportFromTcpSocket socket framing log

def clientTransportFromTcpAddress
    (addr : SocketAddress)
    (framing : Framing.Spec := .newline)
    (log : LogLevel → String → IO Unit := silentLogger) : Async Transport := do
  let socket ← TCP.Socket.Client.mk
  LeanWorker.Transport.logAsync log .info s!"connecting tcp client to {socketAddressToString addr}"
  socket.connect addr
  let peerAddr ← socket.getPeerName
  LeanWorker.Transport.logAsync log .info
    s!"connected tcp client to {socketAddressToString peerAddr}"
  transportFromTcpSocket socket framing log

private partial def connectResolvedAddresses
    (addresses : List SocketAddress)
    (framing : Framing.Spec)
    (log : LogLevel → String → IO Unit) : Async Transport := do
  match addresses with
  | [] =>
    throw <| IO.userError "no tcp addresses available for connection"
  | addr :: rest =>
    try
      clientTransportFromTcpAddress addr framing log
    catch err =>
      LeanWorker.Transport.logAsync log .warn
        s!"tcp connect failed for {socketAddressToString addr}: {err}"
      match rest with
      | [] => throw err
      | _ => connectResolvedAddresses rest framing log

def clientTransportFromTcp
    (endpoint : TcpEndpoint)
    (framing : Framing.Spec := .newline)
    (log : LogLevel → String → IO Unit := silentLogger) : Async Transport := do
  LeanWorker.Transport.logAsync log .info
    s!"resolving tcp endpoint {endpoint.host}:{endpoint.port.toNat}"
  let addresses ← Std.Internal.IO.Async.DNS.getAddrInfo
    endpoint.host
    (serviceName endpoint.port)
    endpoint.family?
  let addresses := addresses.toList.map fun addr => ipAddrToSocketAddress addr endpoint.port
  if addresses.isEmpty then
    throw <| IO.userError s!"no tcp addresses resolved for {endpoint.host}:{endpoint.port.toNat}"
  connectResolvedAddresses addresses framing log

def serverTransportFromTcpAddress
    (addr : SocketAddress)
    (framing : Framing.Spec := .newline)
    (log : LogLevel → String → IO Unit := silentLogger)
    (backlog : UInt32 := 16) : Async Transport := do
  let server ← TCP.Socket.Server.mk
  LeanWorker.Transport.logAsync log .info
    s!"binding tcp server to {socketAddressToString addr}"
  server.bind addr
  server.listen backlog
  let localAddr ← server.getSockName
  LeanWorker.Transport.logAsync log .info
    s!"tcp server listening on {socketAddressToString localAddr}"
  let socket ← server.accept
  let peerAddr ← socket.getPeerName
  LeanWorker.Transport.logAsync log .info
    s!"accepted tcp client from {socketAddressToString peerAddr}"
  serverTransportFromAcceptedTcpSocket socket framing log

end Transport
end LeanWorker
