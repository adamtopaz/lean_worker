# Testing

Lean tests live under `LeanWorkerTest/`.

Run all tests:

```bash
lake build LeanWorkerTest
lake exe run_tests
```

Notes:

- The parse-error test intentionally logs a JSON parse error to stderr.
- HTTP/TCP-specific tests were removed with transport simplification.
