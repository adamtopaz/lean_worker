# Structured Params and Typed Decoding

Structured params live in `LeanWorker/JsonRpc/Structured.lean` and provide typed decoding/encoding for JSON-RPC `params` and results.

## Key Types

- `Json.Structured`: either `.obj` or `.arr`.
- `FromStructured`: decode structured params into a type.
- `ToStructured`: encode a type as structured JSON.

The server and client helpers use `FromStructured` instances to validate parameters and map failures to `Error.invalidParams`.

## Define a Params Type

```lean
open LeanWorker.JsonRpc

structure AddParams where
  a : Int
  b : Int

instance : FromStructured AddParams where
  fromStructured? params := do
    let kvs ←
      match params with
      | .obj kvs => return kvs
      | _ => throw "object params expected"
    let aJson ← JsonRpc.requireField kvs "a"
    let bJson ← JsonRpc.requireField kvs "b"
    let a ← aJson.getInt?
    let b ← bJson.getInt?
    return { a, b }
```

## Encode Structured Results

```lean
structure Info where
  name : String
  version : String

instance : ToStructured Info where
  toStructured info :=
    .obj <| Std.RBMap.fromList compare
      [ ("name", Json.str info.name),
        ("version", Json.str info.version) ]
```

## Error Mapping

When a `FromStructured` instance fails, the server helpers map the failure to:

- `Error.invalidParams` for requests
- No response for notifications

This logic is implemented in `LeanWorker/JsonRpc/Parse.lean` via `decodeParams` and `decodeParams?`.
