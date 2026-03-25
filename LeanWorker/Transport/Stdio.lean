module

public import LeanWorker.Transport.Core

public section

namespace LeanWorker
namespace Transport

open Std.Internal.IO.Async

private def byteSourceOfStream (stream : IO.FS.Stream) : ByteSource where
  recv? _ := do
    -- Pipe-backed `IO.FS.Stream`s behave more predictably for stdio when read
    -- incrementally, and the shared transport core already buffers framed input.
    let chunk ← stream.read 1
    if chunk.isEmpty then
      return none
    else
      return some chunk

private def byteSinkOfStream (stream : IO.FS.Stream) : ByteSink where
  send bytes :=
    stream.write bytes
  flush :=
    stream.flush

def transportFromStreams
    (readStream writeStream : IO.FS.Stream)
    (framing : Framing.Spec := .newline)
    (log : LogLevel → String → IO Unit := silentLogger) : Async Transport :=
  transportFromByteStreams
    (byteSourceOfStream readStream)
    (byteSinkOfStream writeStream)
    framing
    log

def serverTransportFromStreams
    (readStream writeStream : IO.FS.Stream)
    (framing : Framing.Spec := .newline)
    (log : LogLevel → String → IO Unit := silentLogger) : Async Transport :=
  transportFromStreams readStream writeStream framing log

def clientTransportFromStreams
    (readStream writeStream : IO.FS.Stream)
    (framing : Framing.Spec := .newline)
    (log : LogLevel → String → IO Unit := silentLogger) : Async Transport :=
  transportFromStreams readStream writeStream framing log

def serverTransportFromStdio
    (framing : Framing.Spec := .newline)
    (log : LogLevel → String → IO Unit := silentLogger) : Async Transport := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  serverTransportFromStreams stdin stdout framing log

def clientTransportFromStdio
    (framing : Framing.Spec := .newline)
    (log : LogLevel → String → IO Unit := silentLogger) : Async Transport := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  clientTransportFromStreams stdin stdout framing log

end Transport
end LeanWorker
