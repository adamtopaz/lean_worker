module

public import LeanWorker.JsonRpc.Core
public import Std.Internal.Async.Basic

public section

namespace LeanWorker
namespace Client

open Lean
open JsonRpc
open Std.Internal.IO.Async

inductive Kind where
  | request
  | notification

structure Client where
  request : String → Option Json.Structured → EAsync Error Json
  notify : String → Option Json.Structured → EAsync Error Unit
  batch : Array (String × Option Json.Structured × Kind) →
    EAsync Error (Array <| Option <| Except Error Json)
  shutdown : Async Unit

end Client
end LeanWorker
