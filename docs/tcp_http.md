# TCP and HTTP Utilities

TCP transport helpers and HTTP-like utilities are implemented in:

- `LeanWorker/Transport/Tcp.lean`
- `LeanWorker/Http/Types.lean`
- `LeanWorker/Http/Server.lean`
- `LeanWorker/Http/Client.lean`

## TCP ByteTransport

```lean
open LeanWorker.Transport.Tcp

def connect : Async Transport.ByteTransport :=
  connectByteTransport addr

def listen : Async Tcp.Listener :=
  listenByteTransport addr (fun byteTransport => do
    let transport ← Async.framedTransport byteTransport Framing.newline
    Server.run server () state)
```

Each accepted TCP connection gets its own byte transport; you can share a server state by passing the same `Std.Mutex` to each `Server.run` call.

## HTTP-like Server

```lean
open LeanWorker
open Std.Net

def httpAddr : SocketAddress :=
  SocketAddress.v4 { addr := IPv4Addr.ofParts 127 0 0 1, port := 8080 }

def runHttpServer : IO Unit := do
  let config : Http.ServerConfig := { addr := httpAddr }
  let state ← Std.Mutex.new 0
  let listener ← Async.block <| Http.serve config fun transport =>
    Server.run { handlers := handlers, notifications := notifications, transport := transport } () state
  Async.block listener.shutdown
```

## HTTP-like Client

```lean
open LeanWorker
open Std.Net

def httpAddr : SocketAddress :=
  SocketAddress.v4 { addr := IPv4Addr.ofParts 127 0 0 1, port := 8080 }

def main : IO Unit := do
  let config : Http.ClientConfig := { addr := httpAddr }
  let client ← Async.block <| Http.client config
  let result ← EIO.toIO' <| EAsync.block <| client.request "ping" none
  IO.println s!"{result}"
```

## Notes

HTTP-like framing is intended for simple JSON-RPC request/response interactions. It is not a fully compliant HTTP server, but it is compatible with `curl` and basic HTTP clients.
