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

def Nat.log (b n : Nat) : Nat := sorry

def MerkleTreeParams.new (row_size col_size queries: Nat) : MerkleTreeParams := 
let layers := (Nat.log 2 row_size) ;
let top_layer := Nat.min (layers - 1) ((Nat.log 2 queries) + 1) ;
MerkleTreeParams.mk row_size col_size queries layers top_layer (2 ^ top_layer)

def MerkleTreeParams.idx_to_top (self: MerkleTreeParams) (idx: Nat): Nat := idx - self.top_size

def MerkleTreeParams.idx_to_rest (_self: MerkleTreeParams) (idx: Nat): Nat := idx - 1



def HashRawPodSlice (val : Array Elem) : R0sy.Hash.Sha2.Sha256.Digest := sorry -- TODO does this already exist?

def HashPair (a b : Sha256.Digest) : Sha256.Digest := sorry -- TODO does this already exist?



structure MerkleTreeVerifier where
  params: MerkleTreeParams
  top: Subarray Sha256.Digest
  rest: Subarray Sha256.Digest

def MerkleTreeVerifier.root (self: MerkleTreeVerifier): Sha256.Digest
  := if self.rest.size == 0
      then self.top[MerkleTreeParams.idx_to_top self.params 1]!
      else self.rest[MerkleTreeParams.idx_to_rest self.params 1]!

def MerkleTreeVerifier.new [Monad M] [MonadReadIop M] (row_size col_size queries: Nat): 
  M MerkleTreeVerifier := do
  let params := MerkleTreeParams.new row_size col_size queries
  let topArray <- MonadReadIop.readPodSlice params.top_size
  let top := topArray.toSubarray
  let mut rest : Array Sha256.Digest := (List.replicate params.top_size _).toArray
  for i in [params.top_size/2 : params.top_size] do
    let top_idx := params.idx_to_top (2 * i)
    rest := rest.set! i (HashPair (top.get! top_idx) (top.get! (top_idx + 1)))

  sorry
  -- TODO figure out how to reverse this iterator
  -- for i in [1 : params.top_size/2] do
  --   let top_idx := params.idx_to_top (2 * i)
  --   rest := rest.set! i (HashPair (top.get! top_idx) (top.get! (top_idx + 1)))
  
  let verifier := MerkleTreeVerifier.mk params top rest.toSubarray
  MonadReadIop.commit verifier.root
  return verifier


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
