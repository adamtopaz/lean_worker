# Integration Test Harness

Integration scripts live under `scripts/integration/` and exercise the full server over multiple framings.

Run all integration tests:

```bash
scripts/integration/run.sh
```

## Scripts

- `stdio_newline.sh`: launches `full_server` over stdio + newline framing and checks basic requests.
- `stdio_framed_streams.sh`: launches `full_server` over stdio with content-length/http-like framing, checks large payload handling, and validates malformed-header parse errors.
- `tcp_content_length.sh`: uses raw sockets with content-length framing; validates success and error cases.
- `http_like.sh`: uses `curl` to test HTTP-like framing, including a parse-error case.

## Requirements

- `python` or `python3` for stdio and TCP scripts
- `curl` for the HTTP-like script

## Expected Logs

The HTTP-like script sends invalid JSON to verify parse error handling. This produces an expected parse-error line in stderr.
