# Integration Test Harness

Integration scripts are in `scripts/integration/`.

Run:

```bash
scripts/integration/run.sh
```

## Active Coverage

- `stdio_newline.sh`: stdio + newline framing, request/response checks.
- `stdio_framed_streams.sh`: stdio + content-length framing, large payload and framing behavior.

## Legacy Scripts

TCP/HTTP-like scripts remain in `scripts/integration/` as legacy artifacts and are not part of the current supported transport surface.
