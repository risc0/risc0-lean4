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

def HashRawPodSlice (val : Array Elem) : R0sy.Hash.Sha2.Sha256.Digest := sorry -- TODO does this already exist?

def HashPair (a b : Sha256.Digest) : Sha256.Digest := sorry -- TODO does this already exist?

def MerkleTreeVerifier.verify [Monad M] [MonadReadIop M] [MonadExceptOf VerificationError M] [MonadStateOf Nat M] [R0sy.Algebra.Field Elem] 
  (self: MerkleTreeVerifier) (idx_: Nat): M (Array Elem) := do 
  let mut idx := idx_
  if idx >= 2 * self.params.row_size 
    then throw (VerificationError.MerkleQueryOutOfRange idx self.params.row_size) 
    else
  let out <- MonadReadIop.readFields Elem self.params.col_size
  let mut cur := HashRawPodSlice out
  idx := idx + self.params.row_size
  while idx >= 2 * self.params.row_size do
    let low_bit := idx % 2
    let otherArray <- MonadReadIop.readPodSlice 1
    let other := otherArray.get! 0
    idx := idx / 2
    if low_bit == 1 
    then 
      cur := HashPair other cur
    else 
      cur := HashPair cur other
  let present_hash := 
    if idx >= self.params.top_size 
      then self.top.get! (self.params.idx_to_top idx) 
      else self.top.get! (self.params.idx_to_rest idx)
  if present_hash == cur 
  then
    return out
  else 
    throw VerificationError.InvalidProof

end Zkvm.Verify.Merkle
