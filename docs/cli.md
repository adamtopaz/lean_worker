# Full Server CLI

The `full_server` executable runs the expanded test server with configurable transport and framing.

Build:

```bash
lake build full_server
```

Run (stdio + newline):

```bash
lake exe full_server -- --transport stdio --framing newline
```

Run (tcp + http-like):

```bash
lake exe full_server -- --transport tcp --framing http-like --host 127.0.0.1 --port 41000
```

## Flags

- `--transport stdio|tcp`
- `--framing newline|content-length|http-like`
- `--host <host>` (tcp only)
- `--port <port>` (tcp only)
- `--log-level debug|info|warn|error`
- `--max-tasks <n>`
- `--name <string>`
- `--version <string>`
- `--delay-ms <n>`
- `--allow-custom-errors` / `--no-custom-errors`
- `--help`

## Notes

- The HTTP-like framing uses a response start line (`HTTP/1.1 200 OK`) so tools like `curl` can parse responses.
- State is shared across TCP connections by default (a shared `Std.Mutex` is reused).
