# Codebase Review Report

Full audit of the `lean_worker` Lean 4 JSON-RPC framework.
The project builds cleanly (`lake build` -- 89 jobs, zero warnings).

> **Note:** `AGENTS.md` references Lean 4.27.0 but `lean-toolchain` pins `v4.28.0`.
> Update the docs or the toolchain so they agree.

---

## Table of Contents

1. [High Severity](#1-high-severity)
2. [Medium Severity](#2-medium-severity)
3. [Low Severity](#3-low-severity)
4. [Dead Code](#4-dead-code)
5. [Code Duplication](#5-code-duplication)
6. [Test Gaps](#6-test-gaps)

---

## 1. High Severity

### 1.1 Completed handler tasks are never awaited -- exceptions silently dropped

**Location:** `LeanWorker/Server/Run.lean:24-25`

```lean
filterTasks (tasks : Array (AsyncTask Unit)) : BaseIO (Array (AsyncTask Unit)) :=
  tasks.filterM fun task => return (← task.getState) != .finished
```

`filterTasks` discards finished tasks based solely on their state. It never
calls `await` on them, so if a handler threw an exception, that exception is
silently lost. The server has no visibility into handler crashes.

**Fix suggestion:** Await each finished task inside `filterTasks` and log any
exceptions:

```lean
filterTasks (tasks : Array (AsyncTask Unit)) : Async (Array (AsyncTask Unit)) :=
  tasks.filterMapM fun task => do
    if (← task.getState) == .finished then
      try await task catch err =>
        logError s!"handler task failed: {err}"
      return none
    else
      return some task
```

---

### 1.2 Batch requests bypass the `maxTasks` concurrency limit

**Location:** `LeanWorker/Server/Run.lean:92-101`

```lean
handleBatch (state : Std.Mutex State) (items : Array Json) : Async Unit := do
  ...
    let mut tasks : Array (AsyncTask (Option Response)) := #[]
    for item in items do
      tasks := tasks.push <| ← async <| processSingle state item
```

The `mainLoop` respects `server.maxTasks` for individual messages
(`LeanWorker/Server/Run.lean:121-126`), but `handleBatch` spawns one async
task per batch item with no concurrency check. A single large batch bypasses
the limit entirely, which could cause unbounded resource consumption.

**Fix suggestion:** Apply the same `maxTasks` throttle inside `handleBatch`.
Reuse `filterTasks` or a shared semaphore so both single-request and
batch-request paths share one concurrency budget.

---

## 2. Medium Severity

### 2.1 Notification parameter decode errors silently swallowed

**Location:** `LeanWorker/Server/Types.lean:58-63` and `65-70`

```lean
def Notification.ofStateful [FromStructured α]
    (notification : StatefulNotification Context State α) : Notification Context State :=
  .stateful fun params? => do
    match decodeParams? (α := α) params? with
    | .ok value => notification value
    | .error _ => return          -- ← silent drop
```

Both `Notification.ofStateful` and `Notification.ofStateless` discard decode
errors with `| .error _ => return`. No log message, no metric, no trace.
Because JSON-RPC 2.0 forbids responses to notifications, the only recourse
is logging -- but there is none. Debugging malformed clients becomes very
difficult.

**Fix suggestion:** Accept a `log` function (or thread it through the
context) and emit a warning on decode failure:

```lean
| .error msg =>
  -- At minimum, log so operators can diagnose bad clients.
  Transport.logAsync ctx.log .warn s!"notification param decode failed: {msg}"
```

Alternatively, carry a `log` field on `Notification` itself and call it
in the error branch.

---

### 2.2 Server busy-waits when the task limit is reached

**Location:** `LeanWorker/Server/Run.lean:121-126`

```lean
while tasks.size >= max do
  tasks ← filterTasks tasks
  if tasks.size >= max then
    IO.sleep 1
```

The loop polls with a 1 ms sleep. Under sustained load at the task limit this
burns CPU doing nothing useful. The sleep granularity is also too fine for
most schedulers, so actual sleep time may be significantly longer than 1 ms on
some platforms.

**Fix suggestion:** Increase the sleep to 10-50 ms, or replace the polling
loop with a condition variable / semaphore that is signaled when a task
completes.

---

### 2.3 No `try`/`catch` in final task-await loop at shutdown

**Location:** `LeanWorker/Server/Run.lean:20-22`

```lean
let remainingTasks ← mainLoop state
for task in remainingTasks do
  await task
```

If any task throws during `await`, the exception propagates immediately and
the remaining tasks in the array are never awaited, potentially leaking
resources or leaving promises unresolved.

**Fix suggestion:** Wrap each `await` in a `try`/`catch`:

```lean
for task in remainingTasks do
  try await task catch err =>
    logError s!"handler task failed during shutdown: {err}"
```

---

### 2.4 `Error.internalError` used for many distinct failure modes in client

**Location:** `LeanWorker/Client/Run.lean` -- lines 96, 106, 116, 121, 155, 164

Every one of these sites produces the same
`{ code := -32603, message := "Internal error" }` with no distinguishing
data:

| Line | Actual failure mode |
|------|---------------------|
| 96   | Reader task crashed; pending promises bulk-resolved |
| 106  | `sendJson` outbox send failure |
| 116  | `request` outbox send failure |
| 121  | Promise `result?` returned `none` (channel closed) |
| 155  | `batch` outbox send failure |
| 164  | Batch promise `result?` returned `none` |

Callers cannot distinguish a transport failure from a shutdown race from a
reader crash.

**Fix suggestion:** Use `Error.withData` or `Error.withMessage` to attach a
description to each site:

```lean
-- line 106
| .error _ => throw (Error.internalError.withMessage "outbox send failed")

-- line 121
| none => throw (Error.internalError.withMessage "connection closed while awaiting response")
```

---

### 2.5 Child process stdin pipe not explicitly closed on shutdown

**Location:** `LeanWorker/Transport/Spawn.lean:85-101`

After `child.takeStdin` returns the `stdinHandle`, it is wrapped in a
`IO.FS.Stream` and handed to `clientTransportFromStreams`. On shutdown, the
outbox channel is closed and the write loop exits, but the underlying
`stdinHandle` is never explicitly closed. The child process will not see EOF
on its stdin until the handle is garbage-collected, which may delay the
child's own shutdown.

**Fix suggestion:** Close the stdin handle as part of the `shutdownAction`,
before waiting for the child:

```lean
let shutdownAction : Async (Except String Unit) := do
  try stdinHandle.close catch _ => pure ()
  shutdownChild child log config.shutdownTimeoutMs?
```

---

## 3. Low Severity

### 3.1 `ByteArray.get!` in framing code (6 occurrences)

**Locations:**

- `LeanWorker/Framing/Newline.lean:14` -- `line.get! (line.size - 1)`
- `LeanWorker/Framing/Newline.lean:25` -- `buffer.get! index`
- `LeanWorker/Framing/ContentLength.lean:17-20` -- four `buffer.get!` calls

All six are guarded by prior bounds checks (`line.size > 0`,
`index < buffer.size`, `index + 3 < size`), so they are safe today. However
`get!` panics on out-of-bounds access, so any future refactoring that weakens
the guards would introduce a silent runtime panic.

**Fix suggestion:** Replace `get!` with the bounds-checked subscript
`buffer[index]` using `Fin`-based indexing, or use `getD` with a default
value.

---

### 3.2 Error recovery inconsistency between framing modes

**Locations:**

- `LeanWorker/Transport/Streams.lean:109-134` (newline loop)
- `LeanWorker/Transport/Streams.lean:136-165` (content-length loop)

The newline read loop recovers from framing errors and continues reading. The
content-length read loop terminates on any single error (closes the inbox).
The content-length behavior is arguably correct (a bad frame corrupts the
stream position, making recovery impossible), but the asymmetry is
undocumented.

**Fix suggestion:** Add a comment in `readContentLengthLoop` explaining why
recovery is not attempted, e.g.:

```lean
-- Content-Length framing errors corrupt the stream position.
-- Unlike newline framing, recovery is not possible, so we close the inbox.
```

---

### 3.3 No duplicate-method detection in handler/notification registries

**Location:** `LeanWorker/Server/Types.lean:78-82` and `104-108`

```lean
def HandlerRegistry.add ... :=
  { entries := registry.entries.insert method handler }
```

`HashMap.insert` silently overwrites an existing entry. If the same method
name is registered twice, the first handler is silently replaced with no
warning. This could mask configuration bugs.

**Fix suggestion:** Check for existence before insert and either log a
warning or return an error:

```lean
def HandlerRegistry.add ... :=
  if registry.entries.contains method then
    panic! s!"duplicate handler registration for method: {method}"
  else
    { entries := registry.entries.insert method handler }
```

Or, for a softer approach, use a `dbg_trace` warning.

---

### 3.4 No per-handler execution timeout

**Location:** `LeanWorker/Server/Run.lean:31-48` (`runHandler`) and
`LeanWorker/Server/Run.lean:115-130` (`mainLoop`)

There is no timeout on individual handler execution. A handler that loops
forever permanently consumes a task slot. If the handler is stateful, it holds
the `Std.Mutex` forever, deadlocking all subsequent stateful requests.

**Fix suggestion:** Wrap handler execution in a timeout using
`awaitTaskWithTimeout` or an `Async`-level timeout mechanism. On timeout,
return `Error.internalError.withMessage "handler timed out"`. A configurable
`handlerTimeoutMs` field on `Server` would allow callers to set the limit.

---

### 3.5 `writeJsonLoop` accepts a `log` parameter but never uses it

**Location:** `LeanWorker/Transport/Streams.lean:176-186`

```lean
private partial def writeJsonLoop
    (writeStream : IO.FS.Stream)
    (frameSpec : FrameSpec)
    (log : LogLevel → String → IO Unit)     -- ← unused
    (outbox : Std.CloseableChannel Lean.Json) : Async Unit := do
```

Write errors propagate to the caller's `catch` block and are logged there, so
this is not a correctness issue, but the unused parameter is misleading.

**Fix suggestion:** Either remove the `log` parameter (it is not used) or add
per-write error logging inside the loop.

---

### 3.6 `shutdownResult?` is always `some` -- dead match arm

**Location:** `LeanWorker/Transport/Streams.lean:246-279`

```lean
let shutdownResult? ←
  pure (some (← shutdownAction))      -- always `some`
...
| none =>                               -- unreachable
  pure ()
```

The `Option` wrapper around `shutdownAction` was likely intended for a
conditional shutdown, but as written, the `none` branch at line 278 is dead.

**Fix suggestion:** Remove the `Option` wrapper entirely and bind
`shutdownAction` directly:

```lean
let shutdownResult ← shutdownAction
let shutdownError? :=
  match shutdownResult with
  | .error message => some message
  | .ok _ => none
```

---

### 3.7 `shutdownChild` swallows `child.kill` exception without logging the error

**Location:** `LeanWorker/Transport/Spawn.lean:52-55`

```lean
try
  child.kill
catch _ =>
  LeanWorker.Transport.logError log "failed to send terminate signal to child process"
```

The `catch _` discards the actual exception. The log message says the signal
failed but does not say why.

**Fix suggestion:** Capture and log the exception:

```lean
catch err =>
  LeanWorker.Transport.logError log s!"failed to send terminate signal: {err}"
```

---

### 3.8 Stateful notifications hold the mutex during long-running IO

**Location:** `LeanWorker/Server/Run.lean:54-58` and
`LeanWorkerTest/FullServer/Handlers.lean:153-162`

All stateful notifications run inside `state.atomically do`, which holds the
`Std.Mutex` for the entire duration of the notification handler. The test
server's `sleepNotification` calls `IO.sleep` while holding the mutex,
blocking all other stateful operations for the sleep duration.

This is inherent to the `StatefulNotificationM` design (which runs in
`BaseIO` under the mutex), but it means any long-running IO in a stateful
notification is a latent bottleneck.

**Fix suggestion:** Document this constraint prominently in
`Server/Types.lean`. For handlers that need long-running IO, recommend using
`StatelessNotification` and managing state updates separately, or split the
work into a mutex-protected state update followed by a non-mutex IO phase.

---

### 3.9 Redundant inbox close calls during shutdown

**Location:** `LeanWorker/Transport/Streams.lean:230-289`

The `shutdown` closure calls `closeOrLog` on the inbox in up to four separate
code paths (lines 245, 253, 288, and also from the reader/writer async tasks
at lines 222, 228-229). Since `closeOrLog` handles the "already closed" case
gracefully, this is not a bug, but the redundant calls make the control flow
harder to follow.

**Fix suggestion:** Restructure the shutdown sequence to close the inbox
exactly once at the end (after all error checks), removing the intermediate
close calls. Or add a comment noting the intentional redundancy.

---

## 4. Dead Code

| Location | Item | Notes |
|----------|------|-------|
| `LeanWorker/Transport/Logging.lean:17-21` | `logMessage` | Never called anywhere. Trivial wrapper around `log level message`. Remove it. |
| `LeanWorker/JsonRpc/Encoding.lean:92-93` | `parseErrorResponse` | Never referenced. `invalidRequestResponse` is used instead. Remove it. |
| `LeanWorker/Framing/Parse.lean:19` | `\| [] =>` match arm in `parseHeaderLine` | `String.splitOn` never returns an empty list, so this branch is unreachable. Replace with a comment or remove. |
| `LeanWorker/JsonRpc/Structured.lean:17-20` | `ToStructured` class and its instances (lines 39-40) | Defined and exported but never used anywhere in the codebase. Remove or add a consumer. |
| `LeanWorker/JsonRpc/Types.lean` (entire file) | Re-export shim | Misleadingly named "Types" but defines nothing; only re-exports `LeanWorker.JsonRpc`. Consider removing or renaming. |
| `LeanWorker/Transport/Streams.lean:278` | `\| none =>` branch in shutdown | Dead because `shutdownResult?` is always `some` (see 3.6). |

---

## 5. Code Duplication

### 5.1 `awaitTaskWithTimeout` duplicated verbatim

**Locations:**

- `LeanWorker/Transport/Streams.lean:188-204`
- `LeanWorker/Client/Run.lean:20-36`

Both are `private partial def` with identical signatures and bodies.

**Fix suggestion:** Extract to a shared module (e.g., `LeanWorker/Async.lean`
or `LeanWorker/Transport/Util.lean`) and import from both sites.

---

### 5.2 `ServerTransport` and `ClientTransport` are structurally identical

**Location:** `LeanWorker/Transport/Types.lean:23-33`

Both structures have the same four fields with the same types. The factory
functions `serverTransportFromStreams` and `clientTransportFromStreams`
(`LeanWorker/Transport/Streams.lean:292-310`) are also identical in
implementation.

**Fix suggestion:** Define a single `Transport` structure and use type
aliases:

```lean
structure Transport where
  inbox : Std.CloseableChannel (Except Error Lean.Json)
  outbox : Std.CloseableChannel Lean.Json
  log : LogLevel → String → IO Unit
  shutdown : Async (Except String Unit)

abbrev ServerTransport := Transport
abbrev ClientTransport := Transport
```

---

### 5.3 `decodeJsonPayload` / `encodeJsonPayload` duplicated in tests

**Locations:**

- `LeanWorker/Transport/Streams.lean:37-46` (library)
- `LeanWorkerTest/Tests/FramingCodec.lean:14-22` (test)

The test file redefines both functions identically. If the library version
changes, the test copy silently diverges.

**Fix suggestion:** Make the library versions non-private (or expose them
through a test-support module) and import them in the test.

---

### 5.4 `logLevelTag` duplicated in CLI

**Locations:**

- `LeanWorker/Transport/Logging.lean:23-27`
- `LeanWorkerTest/FullServerCli.lean:54-58`

**Fix suggestion:** Import and reuse the library version in the CLI.

---

## 6. Test Gaps

### 6.1 Untested handlers and notifications

Three registered server methods are never exercised by any test:

| Method | Registered at | Type |
|--------|---------------|------|
| `events.list` | `LeanWorkerTest/FullServer/Handlers.lean:178` | Handler |
| `events.clear` | `LeanWorkerTest/FullServer/Handlers.lean:179` | Handler |
| `notify.sleep` | `LeanWorkerTest/FullServer/Handlers.lean:188` | Notification |

**Fix suggestion:** Add test cases in `LeanWorkerTest/Tests/State.lean` or
`LeanWorkerTest/Tests/Notifications.lean` that call these methods and verify
their behavior.

---

### 6.2 Client `batch` API untested

The `Client.Client.batch` method (`LeanWorker/Client/Types.lean:22-23`,
implemented in `LeanWorker/Client/Run.lean:127-164`) is never called by any
test. All batch tests use the raw client approach with manual JSON payloads.

**Fix suggestion:** Add a test in `LeanWorkerTest/Tests/Batch.lean` that
exercises `client.client.batch` with a mix of requests and notifications.

---

### 6.3 Untested JSON-RPC protocol edge cases

The following parse/validation paths are exercised by library code but have no
test coverage:

| Path | Code location | What to test |
|------|---------------|--------------|
| Reserved method names (`rpc.*`) | `LeanWorker/JsonRpc/Parse.lean:60-63` | Send `{"method": "rpc.foo"}`, expect error |
| String-typed RPC IDs | `LeanWorker/JsonRpc/Core.lean:33` (`RpcId.str`) | Send `{"id": "abc"}`, verify round-trip |
| Fractional numeric IDs | `LeanWorker/JsonRpc/Parse.lean:31-36` | Send `{"id": 1.5}`, expect rejection |
| Wrong JSON-RPC version | `LeanWorker/JsonRpc/Parse.lean:49-56` | Send `{"jsonrpc": "1.0"}`, expect error |
| Non-structured `params` | `LeanWorker/JsonRpc/Parse.lean:65-73` | Send `{"params": "string"}`, expect error |
| Response sent to server | `LeanWorker/Server/Run.lean:83-84` | Send a response message, expect `invalidRequestResponse` |
| `maxTasks` throttling | `LeanWorker/Server/Run.lean:121-126` | Configure `maxTasks`, saturate with slow handlers, verify throttling |
| `allowCustomErrors := false` | `LeanWorkerTest/FullServer/Handlers.lean:131-134` | Use `--no-custom-errors` flag, verify error shape |
| Notification with bad params | `LeanWorker/Server/Types.lean:61-63` | Send notification with wrong param types, verify silent ignore (and ideally a log) |
| CRLF in newline framing | `LeanWorker/Framing/Newline.lean:13-17` | Send `\r\n`-terminated payloads, verify correct parsing |

---

### 6.4 Timing-sensitive tests risk CI flakes

| Test | Location | Concern |
|------|----------|---------|
| `testParallelSleep` | `LeanWorkerTest/Tests/State.lean:178` | Asserts `elapsed < ms + 80` (80 ms margin). On slow CI this could flake. |
| `waitForSnapshot` | `LeanWorkerTest/Tests/Notifications.lean:45` | Polls 20 times at 10 ms (200 ms budget). May be tight under load. |

**Fix suggestion:** Widen the margin for `testParallelSleep` (e.g.,
`elapsed < 2 * ms`) and increase `waitForSnapshot` budget on CI.

---

### 6.5 Sequential test execution with abort-on-first-failure

**Location:** `LeanWorkerTest/Main.lean`

The test runner calls each test sequentially. If one test fails (throws
`IO.userError`), all subsequent tests are skipped and no summary is printed.

**Fix suggestion:** Wrap each `runTest` call in a `try`/`catch`, accumulate
pass/fail counts, and print a summary at the end. Return a non-zero exit code
if any test failed.
