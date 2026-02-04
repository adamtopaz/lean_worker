# Client Runtime

The client runtime is implemented in `LeanWorker/Client.lean`.

## Client API

```lean
structure Client where
  request : String → Option Json.Structured → EAsync Error Json
  notify : String → Option Json.Structured → EAsync Error Unit
  batch : Array (Message) → EAsync Error (Array (Option (Except Error Json)))
  shutdown : Async Unit
```

The client tracks pending requests via `Std.Mutex` + `IO.Promise` and resolves responses asynchronously.

## Basic Usage

```lean
open LeanWorker

def main : IO Unit := do
  let client ← Async.block <| Client.spawnStdioClient { cmd := "./server" }
  let result ← EIO.toIO' <| EAsync.block <| client.request "ping" none
  IO.println s!"{result}"
```

## Batch Requests

Use `Client.batch` to send a mix of requests and notifications. Responses are returned as an array of options, aligned with input order (notifications yield `none`).

```lean
open LeanWorker
open LeanWorker.JsonRpc

def runBatch (client : Client.Client) : EAsync Error Unit := do
  let req : Message := .request { id := RpcId.num 1, method := "ping", params? := none }
  let note : Message := .notification { method := "notify.bump", params? := none }
  let results ← client.batch #[req, note]
  match results[0]? with
  | some (Except.ok json) =>
    IO.println s!"ping: {json}"
  | _ =>
    throw Error.internalError
  return ()
```

## Shutdown

`Client.shutdown` closes the outbox, shuts down the transport, and resolves any pending promises with `Error.internalError`.
