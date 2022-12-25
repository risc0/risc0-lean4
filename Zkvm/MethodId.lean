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

partial def MethodId.ofWords (D: Type) [Hash D] (words: Subarray UInt32) (table: Array D := Array.mkEmpty (words.size / 8)): MethodId D
  := if 8 < words.size
      then
        let (digest, words') := Subarray.take words (SerialUInt32.words D)
        MethodId.ofWords D words' (table.push <| SerialUInt32.fromUInt32Words digest)
      else { table }

end Zkvm.MethodId
