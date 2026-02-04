module

public import LeanWorkerTest.Support

public section

def main : IO Unit := do
  LeanWorkerTest.withManagedClient fun client => do
    let result ← LeanWorkerTest.runEAsync <| client.client.request "ping" none
    match result with
    | .ok _ => return
    | .error err =>
      throw <| IO.userError s!"client ping failed: {err.code}"
