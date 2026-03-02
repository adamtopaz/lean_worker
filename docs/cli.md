# Full Server CLI

The `full_server` executable runs the expanded test server.

Build:

```bash
lake build full_server
```

Run:

```bash
lake exe full_server -- --transport stdio --framing newline
```

or:

```bash
lake exe full_server -- --transport stdio --framing content-length
```

## Flags

- `--transport stdio`
- `--framing newline|content-length`
- `--log-level debug|info|warn|error`
- `--max-tasks <n>`
- `--name <string>`
- `--version <string>`
- `--delay-ms <n>`
- `--allow-custom-errors` / `--no-custom-errors`
- `--help`
