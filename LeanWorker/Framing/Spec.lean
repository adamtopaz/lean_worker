module

public import LeanWorker.Framing.Newline
public import LeanWorker.Framing.ContentLength

public section

namespace LeanWorker
namespace Framing

open Lean
open JsonRpc

inductive Spec where
  | newline
  | contentLength
 deriving BEq, Repr, Inhabited

structure Codec where
  encode : ByteArray → ByteArray
  decode : ByteArray → Except Error (Array ByteArray × ByteArray)
  pullPayload? : ByteArray → Except Error (Option (ByteArray × ByteArray))
  eofError : Error

private def eofErrorFor : Spec → Error
  | .newline => framingError "unexpected EOF while reading newline-framed payload"
  | .contentLength => framingError "unexpected EOF while reading content-length-framed payload"

def codec : Spec → Codec
  | .newline =>
    {
      encode := encodeNewlineBytes
      decode := decodeNewlineBytes
      pullPayload? := pullNewlinePayload?
      eofError := eofErrorFor .newline
    }
  | .contentLength =>
    {
      encode := encodeContentLengthBytes
      decode := decodeContentLengthBytes
      pullPayload? := pullContentLengthPayload?
      eofError := eofErrorFor .contentLength
    }

@[inline] def encode (spec : Spec) : ByteArray → ByteArray :=
  (codec spec).encode

@[inline] def decode
    (spec : Spec) : ByteArray → Except Error (Array ByteArray × ByteArray) :=
  (codec spec).decode

@[inline] def pullPayload?
    (spec : Spec) : ByteArray → Except Error (Option (ByteArray × ByteArray)) :=
  (codec spec).pullPayload?

@[inline] def eofError (spec : Spec) : Error :=
  (codec spec).eofError

end Framing
end LeanWorker
