# Expanded Test Server

The expanded test server lives under:

- `LeanWorkerTest/FullServer/Types.lean`
- `LeanWorkerTest/FullServer/Handlers.lean`
- `LeanWorkerTest/FullServer.lean`

It is used by the CLI (`full_server`) and integration scripts.

## Context and State

- `FullContext`: `name`, `version`, `defaultDelayMs`, `allowCustomErrors`
- `FullState`: `counter`, `kvs`, `events`, `inFlight`

## Requests

- `ping` -> "pong"
- `info` -> `{ name, version, defaultDelayMs }`
- `echo` -> params as JSON
- `add` -> expects `{ a: Int, b: Int }`
- `sum` -> expects `[Int]`
- `sleep` -> expects `{ ms: Nat, tag?: String }`, returns `{ ms, tag? }`
- `counter.get` -> current counter
- `counter.add` -> expects `{ delta: Int }` (negative rejected)
- `counter.reset` -> returns previous counter
- `kv.get` -> expects `{ key: String }`, returns `{ found: Bool, value? }`
- `kv.set` -> expects `{ key: String, value: Json }`, returns `{ hadValue: Bool, oldValue? }`
- `kv.delete` -> expects `{ key: String }`, returns `Bool`
- `events.list` -> array of strings
- `events.clear` -> count cleared
- `state.snapshot` -> `{ counter, kvSize, eventsSize, inFlight }`
- `error.custom` -> expects `{ code: Int, message: String, data?: Json }`
- `error.internal` -> returns `Internal error`

## Notifications

- `notify.bump` -> `{ delta?: Nat }` increments counter
- `notify.log` -> `{ message: String }` appends to events
- `notify.sleep` -> `{ ms: Nat }` delays without response
