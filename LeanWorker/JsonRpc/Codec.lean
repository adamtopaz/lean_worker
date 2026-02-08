module

public import LeanWorker.Transport.Codec
public import LeanWorker.JsonRpc.Parse

public section

namespace LeanWorker
namespace JsonRpc

open Lean

private def utf8StringFromBytes (bytes : ByteArray) : Except Error String :=
  match String.fromUTF8? bytes with
  | some text => return text
  | none =>
    throw <| Error.withData Error.parseError (Json.str "invalid UTF-8 in JSON payload")

def jsonCodec : Transport.Codec Json Json :=
  {
    decode := fun bytes => do
      let text ← utf8StringFromBytes bytes
      parseJson text,
    encode := fun json => (Json.compress json).toUTF8
  }

end JsonRpc
end LeanWorker
