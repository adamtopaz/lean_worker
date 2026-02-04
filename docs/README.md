# LeanWorker Documentation

This directory contains detailed documentation for each component of the LeanWorker JSON-RPC framework.

## Quick Start

Build everything:

```bash
lake build
```

Run the full test suite:

```bash
scripts/integration/run.sh
lake exe run_tests
```

Start the full test server over TCP + HTTP-like framing:

```bash
lake exe full_server -- --transport tcp --framing http-like --host 127.0.0.1 --port 41000
```

Send a request:

```bash
curl -sS --http1.1 -H 'Content-Type: application/json' -H 'Expect:' \
  --data '{"jsonrpc":"2.0","id":1,"method":"ping"}' \
  http://127.0.0.1:41000/
```

## Documentation Index

- `docs/overview.md`: architecture and message flow
- `docs/jsonrpc.md`: JSON-RPC types, validation, and errors
- `docs/structured.md`: structured params helpers
- `docs/transport.md`: transport abstraction
- `docs/framing.md`: newline/content-length/http-like framing
- `docs/async_loops.md`: async read/write loops
- `docs/server.md`: server runtime, handlers, and notifications
- `docs/client.md`: client runtime and request APIs
- `docs/tcp_http.md`: TCP transport and HTTP utilities
- `docs/test_server.md`: expanded test server API
- `docs/cli.md`: `full_server` CLI
- `docs/integration.md`: integration scripts and expected outputs
- `docs/testing.md`: Lean test suite
