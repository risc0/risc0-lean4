/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy

namespace Zkvm.MethodId

open R0sy.Hash
open R0sy.Lean.Subarray
open R0sy.Serial

structure MethodId (D: Type) where
  table: Array D
  deriving Inhabited

def MethodId.ofWords (D: Type) [Hash D] (words: Subarray UInt32): MethodId D
  := Id.run do
        let num_digest := words.size / SerialUInt32.words D
        let mut buf := words
        let mut table: Array D := Array.mkEmpty num_digest
        for _ in [0:num_digest] do
          let digest_buf := Subarray.take buf (SerialUInt32.words D)
          buf := digest_buf.2
          table := table.push <| SerialUInt32.fromUInt32Words digest_buf.1
        pure { table }

end Zkvm.MethodId
