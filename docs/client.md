# Client Runtime

The client runtime is implemented in `LeanWorker/Client.lean`.

## Client API

```lean
structure Client where
  request : String → Option Json.Structured → EAsync Error Json
  notify : String → Option Json.Structured → EAsync Error Unit
  batch : Array (String × Option Json.Structured × Kind) →
    EAsync Error (Array (Option (Except Error Json)))
  shutdown : Async Unit
```

The client tracks pending requests via `Std.Mutex` + `IO.Promise` and resolves responses asynchronously.

## Basic Usage

```lean
open LeanWorker

def main : IO Unit := do
  let client ← Async.block <| Client.spawnStdioClient ({ cmd := "./server" } : Transport.SpawnConfig)
  let result ← EIO.toIO' <| EAsync.block <| client.request "ping" none
  IO.println s!"{result}"
```

## Batch Requests

Use `Client.batch` to send a mix of requests and notifications. Responses are returned as an array of options, aligned with input order (notifications yield `none`).

```lean
open LeanWorker
open LeanWorker.Client
open LeanWorker.JsonRpc

def runBatch (client : Client.Client) : EAsync Error Unit := do
  let items : Array (String × Option Json.Structured × Kind) :=
    #[
      ("ping", none, .request),
      ("notify.bump", none, .notification)
    ]
  let results ← client.batch items
  match results[0]? with
  | some (Except.ok json) =>
    IO.println s!"ping: {json}"
  | _ =>
    throw Error.internalError
  return ()
```

## Shutdown

`Client.shutdown` closes the outbox, shuts down the transport, and resolves any pending promises with `Error.internalError`.
