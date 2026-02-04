# Testing

Lean tests are defined under `LeanWorkerTest/` and run via the test runner executable.

## Run Tests

```bash
lake build LeanWorkerTest
lake exe run_tests
```

## Integration Suite

```bash
scripts/integration/run.sh
```

## Notes

- The parse-error test intentionally logs a framing decode error to stderr (expected).
- The HTTP-like integration test sends invalid JSON and logs a parse error (expected).
