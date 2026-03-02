# AGENTS

This repository is a Lean 4 project built with Lake.
Use this file as the single source of truth for build/test and style conventions.
Prefer minimal, targeted edits and keep formatting consistent with existing Lean code.

## Project Overview
- Language: Lean 4 (see `lean-toolchain`, currently `leanprover/lean4:v4.28.0`)
- Build system: Lake (`lakefile.lean`, `lake-manifest.json`)
- Libraries: `LeanWorker`, `LeanWorkerTest`
- Executables: none currently declared in `lakefile.lean`
- CI: GitHub Actions with `leanprover/lean-action` + `lake build`

## Build, Lint, Test

### Prerequisites
- Install Lean via elan and respect `lean-toolchain`.
- Use `lake` for all builds; avoid running `lean` directly without `lake env`.
- Fetch deps once: `lake update`

### Build
- Build everything: `lake build`
- Build main library: `lake build LeanWorker`
- Build test library target: `lake build LeanWorkerTest`

### Run
- No `lake exe` targets are currently defined.

### Lint
- No standalone linter configured.
- Treat `lake build` as compile/lint signal and fix warnings.

### Tests
- There is currently no runtime test executable in this repo.
- Use compile checks:
  - `lake build`
  - `lake build LeanWorkerTest`

### Single File / Fast Check
- Typecheck one file quickly: `lake env lean LeanWorker/JsonRpc/Core.lean`
- Typecheck one library target: `lake build LeanWorker` (or `LeanWorkerTest`)

### CI
- Keep local verification aligned with CI:
  - `lake build`

## Repo Layout
- `LeanWorker/` contains JSON-RPC core, parsing/encoding, framing, transport, client, and server modules.
- `LeanWorker.lean` re-exports the public LeanWorker API.
- `LeanWorkerTest/` is present as a Lake library target and currently contains no committed modules.
- `.github/workflows/lean_action_ci.yml` defines CI.
- `opencode.json` configures LSP (`lake serve`).

## Tooling
- LSP for Lean: `lake serve` (see `opencode.json`).
- Build artifacts live in `.lake/`; do not edit or commit them.
- Use `lake env <cmd>` when running Lean tools outside Lake.

## Dependency Management
- Add new deps in `lakefile.lean` and run `lake update`.
- Commit updated `lake-manifest.json` when deps change.
- Do not edit `lake-manifest.json` by hand.
- Keep `lean-toolchain` pinned unless intentionally upgrading Lean.

## Code Style (Lean)

### General
- Match existing structure: `module` line, blank line, imports, `public section`, namespaces, then definitions.
- Keep files small and focused; prefer new modules over huge files.
- Close namespaces explicitly with `end <Name>`.
- Keep namespace nesting shallow and aligned with folder/module names.

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
- Keep line length reasonable; break long expressions across lines.
- Keep blank lines between major sections.

### Naming
- Types/structures/inductives: `PascalCase` (e.g., `RpcId`, `Transport`).
- Functions/defs/values: `lowerCamel` (e.g., `parseMessage`, `logError`).
- Namespaces: `PascalCase` matching file/module names.
- Use `?` suffix for partial/optional results (`fromJson?`, `data?`).

### Types and Abbrevs
- Use `structure` for data shapes and `inductive` for sum types.
- Prefer `abbrev` for simple aliases.
- Keep explicit type signatures on public APIs and key helpers.
- Use `Option` for optional fields and represent absence as `none`.

### Pattern Matching
- Exhaustively handle `Option` and `Except` cases.
- Prefer `match` over nested `if` for multi-branch logic.
- Use `if let` only for simple single-branch checks.

### Error Handling
- Use domain errors (`LeanWorker.JsonRpc.Error`) rather than stringly exceptions.
- For JSON parsing, use `FromJson` / `FromStructured` and return structured errors.
- Avoid `IO.println` for operational errors; use transport logging.

### JSON and Serialization
- Implement both `ToJson` and `FromJson` when a type crosses the wire.
- Use `Json.Structured` for structured params and `Option` for optional fields.
- Keep JSON keys stable when changing request/response shapes.

### Async and Concurrency
- Use `Async`, `EAsync`, and `AsyncTask` consistently; await spawned tasks.
- Use `Mutex` for shared mutable state where needed.
- Close channels gracefully and resolve pending promises on shutdown.

### IO and Logging
- Use transport logging for operational messages.
- Keep protocol payloads separate from operational logs.
- Flush streams when interacting with stdio transports.

### Public API
- Re-export shared modules in `LeanWorker.lean` using `public import`.
- Keep `public section` minimal.
- If a public wire type changes, update associated `ToJson` / `FromJson` instances.

## Cursor/Copilot Rules
- No `.cursor/rules/`, `.cursorrules`, or `.github/copilot-instructions.md` are present.
- If such files are added later, follow them in preference to this document.
