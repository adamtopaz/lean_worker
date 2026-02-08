# TCP and HTTP Utilities

TCP helpers and HTTP wrappers are implemented in:

- `LeanWorker/Transport/Tcp.lean`
- `LeanWorker/Http/Types.lean`
- `LeanWorker/Http/Server.lean`
- `LeanWorker/Http/Client.lean`

## TCP Typed Transport

Use typed constructors directly.

```lean
open LeanWorker
open LeanWorker.Transport.Tcp

def connect : Async (Transport.Transport (Except JsonRpc.Error Lean.Json) Lean.Json) :=
  connectJsonTransport addr .contentLength

def listen : Async Tcp.Listener :=
  listenJsonTransport addr (fun transport => do
    Server.run server () state)
  .contentLength
```

For custom payload types, use:

- `Tcp.connectTransport addr frameSpec codec`
- `Tcp.listenTransport addr handle frameSpec codec`

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

HTTP-like mode is intended for simple request/response compatibility (for example with `curl`); it is not a full HTTP server implementation.
