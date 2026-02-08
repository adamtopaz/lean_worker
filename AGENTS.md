# AGENTS

This repository is a Lean 4 project built with Lake.
Use this file as the single source of truth for build/test and style conventions.
Prefer minimal, targeted edits and keep formatting consistent with existing Lean code.

## Project Overview
- Language: Lean 4 (see lean-toolchain)
- Build system: Lake (`lakefile.lean`, `lake-manifest.json`)
- Libraries: LeanWorker, LeanWorkerTest
- Executables: lean_worker (Main.lean), test_server (LeanWorkerTest.TestServer),
  test_client (LeanWorkerTest.TestClient), run_tests (LeanWorkerTest.Main),
  full_server (LeanWorkerTest.FullServerCli)
- CI: GitHub Actions with lean-action (default build)

## Build, Lint, Test

### Prerequisites
- Install Lean via elan and respect lean-toolchain (Lean 4.27.0).
- Use `lake` for all builds; avoid hand-running `lean` without `lake env`.
- Fetch deps once: `lake update`

### Build
- Build everything: `lake build`
- Build the executable: `lake build lean_worker`
- Build the main library: `lake build LeanWorker`
- Build test library: `lake build LeanWorkerTest`
- Build test server: `lake build test_server`
- Build test client: `lake build test_client`
- Build test runner: `lake build run_tests`
- Build full server CLI: `lake build full_server`

### Run
- Run the executable: `lake exe lean_worker`
- Run the test suite: `lake exe run_tests`
- Run the full server CLI: `lake exe full_server -- --transport stdio --framing newline`

### Lint
- No standalone linter configured.
- Treat `lake build` as the compile/lint signal and fix any warnings.

### Tests
- Tests live as Lean modules under `LeanWorkerTest`.
- Run all tests: `lake exe run_tests`
- Typecheck tests only: `lake build LeanWorkerTest`

### Integration
- Run integration scripts: `scripts/integration/run.sh`
- Requirements: `python` or `python3`, plus `curl` for HTTP-like tests.

### Single Test / Single File
- Fast check a single file: `lake env lean LeanWorkerTest/TestServer.lean`
- Fast check a library module: `lake build LeanWorkerTest` (or `LeanWorker`)
- If you add a new test module, update `lakefile.lean` targets/imports if needed.

### CI
- GitHub Actions uses `leanprover/lean-action` (defaults to `lake build`).
- Keep local commands aligned with CI.

## Repo Layout
- `Main.lean` defines the `lean_worker` entry point.
- `LeanWorker/` contains JSON-RPC core, transport, framing, async loops, client, and server.
- `LeanWorkerTest/` contains test server, client harness, and support utilities.
- `LeanWorkerTest/FullServer*.lean` contains the expanded test server and CLI entrypoint.
- `scripts/integration/` contains integration test harness scripts.
- `opencode.json` configures LSP (`lake serve`).

## Tooling
- LSP for Lean: `lake serve` (see `opencode.json`).
- Build artifacts live in `.lake/`; do not edit or commit them.
- Use `lake env <cmd>` when running Lean tools outside Lake.

## Dependency Management
- Add new deps in `lakefile.lean` and run `lake update`.
- Commit the updated `lake-manifest.json` when deps change.
- Do not edit `lake-manifest.json` by hand.
- Keep `lean-toolchain` pinned unless intentionally upgrading Lean.

## Code Style (Lean)

### General
- Match existing structure: `module` line, blank line, imports, `public section`, namespaces, then definitions.
- Keep files small and focused; prefer new modules over huge files.
- Close namespaces explicitly with `end <Name>`.
- Keep `namespace` nesting shallow and aligned with folder/module names.

### Imports
- Put `import` or `public import` statements at the top after `module`.
- Order imports from external (`Lean`, `Std`) to local (`LeanWorker`).
- Use `public import` only when re-exporting from umbrella modules.
- Avoid unused imports; keep `open` statements local to the namespace using them.

### Formatting
- Two-space indentation, especially in `do` and `match` blocks.
- Align `match` arms with `|` and keep patterns on one line when possible.
- Use spaces around `:` and `:=` and after commas.
- Prefer `<|` and `|>` for readability in nested calls, as in `LeanWorker/Common.lean`.
- Keep line length reasonable; break long expressions across lines rather than chaining deeply.
- Keep blank lines between major sections (imports, namespaces, large defs).

### Naming
- Types/structures/inductives: `PascalCase` (e.g., `RpcId`, `Transport`).
- Functions/defs/values: `lowerCamel` (e.g., `messageFromJson`, `getClient`).
- Namespaces: `PascalCase` matching file/module names.
- Use `?` suffix for partial/optional results (`fromJson?`, `data?`).
- Use descriptive abbreviations only when common (`ctx`, `params`, `kvs`).
- Prefer verb names for effects (`sendLog`, `resolveMessage`).

### Types and Abbrevs
- Use `structure` for data shapes and `inductive` for sum types.
- Prefer `abbrev` for simple aliases (see `StatefulHandlerM`).
- Keep explicit type signatures on public API and key helpers.
- Use `Option` for optional fields and store absence as `none`.

### Pattern Matching
- Exhaustively handle `Option` and `Except` cases.
- Prefer `match` over nested `if` for multi-branch logic.
- Use `if let` only for simple single-branch checks.

### Error Handling
- Use domain errors (`LeanWorker.JsonRpc.Error`) rather than stringly-typed exceptions.
- For JSON parsing, use `FromJson` / `FromStructured` instances and return `Except String _`.
- Use `throw .invalidParams` for bad inputs and map parse errors to `Error.parseError`.
- Avoid throwing raw strings in core logic; wrap in structured error types.
- Log channel/transport errors via `Transport.log`, not `IO.println`.

### JSON and Serialization
- Implement both `ToJson` and `FromJson` when a type crosses the wire.
- Use `Json.Structured` for typed params and keep `Option` for optional params.
- Keep JSON keys stable and documented when changing request/response shapes.
- Use `json% { ... }` syntax for concise JSON literals.

### Async and Concurrency
- Use `Async`, `BaseAsync`, and `AsyncTask` consistently; `await` each task.
- Use `Mutex` for shared state (`State` in server handlers).
- Close channels gracefully; log channel errors via `Transport.log`.
- Resolve pending promises on shutdown to avoid leaks.

### IO and Logging
- Use `Transport.log` for operational messages; keep stdout for JSON-RPC payloads.
- Flush streams after writes when interacting with stdio transports.
- Avoid long-running blocking calls in async loops; prefer `IO.sleep` for waits.

### State and Effects
- Use `ReaderT` for context and `StateRefT` for mutable state, as in `Server`.
- Keep handler signatures explicit about effects (`EIO Error`, `BaseIO`).
- When updating state, use `set` and return updated values explicitly.

### Public API
- Re-export shared modules in `LeanWorker.lean` using `public import`.
- Keep `public section` minimal; private helpers can remain local.
- If you change a public type, update any `ToJson` / `FromJson` instances.

### Tests and Examples
- `LeanWorkerTest` modules should be deterministic and fast to typecheck.
- Prefer small handler examples with explicit `FromStructured` instances.
- Keep fixtures focused on JSON-RPC behavior, not integration.
- Add new test modules under `LeanWorkerTest/` and keep them importable.

## Cursor/Copilot Rules
- No `.cursor/rules/`, `.cursorrules`, or `.github/copilot-instructions.md` found.
- If such files are added later, follow them in preference to this document.
