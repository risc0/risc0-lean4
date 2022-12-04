/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy

namespace Zkvm.MethodId

open R0sy.Hash.Sha2
open R0sy.Lean.Subarray
open R0sy.Serial

structure MethodId where
  table: Array Sha256.Digest
  deriving Inhabited

partial def MethodId.ofWords (words: Subarray UInt32) (table: Array Sha256.Digest := Array.mkEmpty (words.size / 8)): MethodId
  := if 8 < words.size
      then
        let (digest, words') := Subarray.take words 8
        MethodId.ofWords words' (table.push <| Sha256.Digest.ofSubarray digest)
      else { table }

end Zkvm.MethodId
