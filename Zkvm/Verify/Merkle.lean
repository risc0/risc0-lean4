/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Hash.Sha2
import Zkvm.Verify.Classes

namespace Zkvm.Verify.Merkle

open R0sy.Hash.Sha2
open Classes

structure MerkleTreeParams where
  row_size: Nat
  col_size: Nat
  queries: Nat
  layers: Nat
  top_layer: Nat
  top_size: Nat

def MerkleTreeParams.idx_to_top (self: MerkleTreeParams) (idx: Nat): Nat := idx - self.top_size

def MerkleTreeParams.idx_to_rest (_self: MerkleTreeParams) (idx: Nat): Nat := idx - 1


structure MerkleTreeVerifier where
  params: MerkleTreeParams
  top: Subarray Sha256.Digest
  rest: Subarray Sha256.Digest

def MerkleTreeVerifier.new [Monad M] [MonadReadIop M] (row_size col_size queries: Nat): MerkleTreeVerifier := sorry

def MerkleTreeVerifier.root (self: MerkleTreeVerifier): Sha256.Digest
  := if self.rest.size == 0
      then self.top[MerkleTreeParams.idx_to_top self.params 1]!
      else self.rest[MerkleTreeParams.idx_to_rest self.params 1]!

def MerkleTreeVerifier.verify [Monad M] [MonadReadIop M] [MonadExceptOf VerificationError M] [MonadStateOf Nat M]: M Elem
  := do let mut idx: Nat <- get
        /- TODO: perform the verification ;) -/
        set idx
        sorry

end Zkvm.Verify.Merkle
