module

public import LeanWorkerTest.Support
public import LeanWorkerTest.Tests.Support

public section

namespace LeanWorkerTest

open Lean
open LeanWorker

partial def waitForSnapshot
    (client : Client.Client)
    (attempts : Nat)
    (delayMs : Nat)
    (predicate : Json → IO Bool) : IO Json := do
  let rec loop : Nat → IO Json
    | 0 => throw <| IO.userError "timeout waiting for state snapshot"
    | remaining => do
      let result ← runEAsync <| client.request "state.snapshot" none
      match result with
      | .ok json =>
        if (← predicate json) then
          return json
        else
          sleepMs delayMs
          loop (remaining - 1)
      | .error err =>
        throw <| IO.userError s!"snapshot failed: {err.code}"
  loop attempts

def testNotifications : IO Unit :=
  withManagedClient fun client => do
    let bumpParams := objParams [("delta", Json.num 2)]
    let logParams := objParams [("message", Json.str "hello")]
    let bumpResult ← runEAsync <| client.client.notify "notify.bump" (some bumpParams)
    match bumpResult with
    | .ok _ => pure ()
    | .error err => throw <| IO.userError s!"notify.bump failed: {err.code}"
    let logResult ← runEAsync <| client.client.notify "notify.log" (some logParams)
    match logResult with
    | .ok _ => pure ()
    | .error err => throw <| IO.userError s!"notify.log failed: {err.code}"
    let snapshot ← waitForSnapshot client.client 20 10 fun json => do
      let counter ← jsonToNat (← expectObjField json "counter")
      let eventsSize ← jsonToNat (← expectObjField json "eventsSize")
      return counter >= 2 && eventsSize >= 1
    let counter ← jsonToNat (← expectObjField snapshot "counter")
    let eventsSize ← jsonToNat (← expectObjField snapshot "eventsSize")
    assert (counter == 2) "unexpected counter after notifications"
    assert (eventsSize == 1) "unexpected events size after notifications"

end LeanWorkerTest
