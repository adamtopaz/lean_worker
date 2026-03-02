# LeanWorker Documentation

LeanWorker docs cover JSON-RPC runtime infrastructure only.

## Quick Start

```bash
lake build
lake exe run_tests
```

Run full test server (stdio + newline):

```bash
lake exe full_server -- --transport stdio --framing newline
```

Run full test server (stdio + content-length):

```bash
lake exe full_server -- --transport stdio --framing content-length
```

## Documentation Index

- `docs/overview.md`: architecture and data flow
- `docs/jsonrpc.md`: JSON-RPC types, validation, errors
- `docs/structured.md`: structured params helpers
- `docs/transport.md`: role-specific transports and constructors
- `docs/framing.md`: newline/content-length framing
- `docs/server.md`: server runtime and handler model
- `docs/client.md`: client runtime and APIs
- `docs/test_server.md`: expanded test server API
- `docs/cli.md`: `full_server` CLI usage
- `docs/integration.md`: integration scripts
- `docs/testing.md`: Lean test suite
