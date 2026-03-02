# Transport

Transports are role-specific and JSON-facing.

Defined in `LeanWorker/Transport/Types.lean`:

```lean
structure ServerTransport where
  inbox : Std.CloseableChannel (Except JsonRpc.Error Lean.Json)
  outbox : Std.CloseableChannel Lean.Json
  log : LogLevel → String → IO Unit
  shutdown : Async Unit

structure ClientTransport where
  inbox : Std.CloseableChannel (Except JsonRpc.Error Lean.Json)
  outbox : Std.CloseableChannel Lean.Json
  log : LogLevel → String → IO Unit
  shutdown : Async Unit
```

## Constructors

- `Transport.serverTransportFromStreams`
- `Transport.clientTransportFromStreams`
- `Transport.spawnStdioClientTransport`
- `Transport.spawnStdioServerTransport`

These constructors apply framing + JSON decode/encode internally and expose JSON-level channels to server/client runtimes.

## Notes

- No generic `Transport Incoming Outgoing` abstraction.
- No generic transport codec abstraction.
- No TCP transport support in this repository at the moment.
