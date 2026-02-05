# JSON-RPC Types and Validation

The core JSON-RPC model is defined in `LeanWorker/JsonRpc/Core.lean`, with parsing helpers in `LeanWorker/JsonRpc/Parse.lean` and encoding helpers in `LeanWorker/JsonRpc/Encoding.lean`.

## Core Types

- `RpcId`: string, number (integer-only), or null.
- `Error`: structured error object with `code`, `message`, and optional `data`.
- `Request`: `{ jsonrpc, id, method, params? }`.
- `Notification`: `{ jsonrpc, method, params? }`.
- `Response`: `{ jsonrpc, id, result }` or `{ jsonrpc, id, error }`.
- `Message`: sum of request/notification/response.
- `Batch`: non-empty array of messages.

## Validation Rules

Validation is applied during parsing (`FromJson` instances):

- `jsonrpc` must be exactly "2.0" for requests and responses.
- `method` must be a string and must not start with `"rpc."`.
- `params` must be `Json.Structured` (array/object) if present.
- `id` must be string/number/null; numbers must be integers.
- `Response` must include exactly one of `result` or `error`.
- `Batch` must be a non-empty array.

## Parsing Helpers

The module provides helpers that map parse/validation errors to JSON-RPC errors:

- `parseJson : String → Except Error Json`
- `parseMessage : Json → Except Error Message`
- `parseBatch : Json → Except Error (Batch Message)`

These helpers use `Error.parseError` and `Error.invalidRequest` with structured data in `data?` for debugging.

## Response Helpers

- `responseResult : RpcId → Json → Response`
- `responseError : RpcId → Error → Response`
- `parseErrorResponse : Response`
- `invalidRequestResponse : Response`

## Example

```lean
open LeanWorker.JsonRpc

def decode (payload : String) : Except Error Response := do
  let json ← parseJson payload
  let msg ← parseMessage json
  match msg with
  | .request req =>
    return responseResult req.id (Json.str "ok")
  | .notification _ =>
    return responseResult RpcId.null Json.null
  | .response resp =>
    return resp
```
