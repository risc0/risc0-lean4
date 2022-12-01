/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Hash
import R0sy.Hash.Sha2
import R0sy.Lean.Nat
import Zkvm.Verify.Classes

namespace Zkvm.Verify.Merkle

open R0sy.Hash
open R0sy.Hash.Sha2
open R0sy.Lean.Nat
open Classes

structure MerkleTreeParams where
  row_size: Nat
  col_size: Nat
  queries: Nat
  layers: Nat
  top_layer: Nat
  top_size: Nat
  deriving Inhabited

def MerkleTreeParams.new (row_size col_size queries: Nat): MerkleTreeParams :=
  let layers: Nat := Nat.log2_floor row_size
  let top_layer: Nat := Nat.log2_floor queries
  let top_size: Nat := 1 <<< top_layer
  if 1 <<< layers != row_size
  then panic "row_size not a power of 2"
  else
    {
      row_size,
      col_size,
      queries,
      layers,
      top_layer,
      top_size
    }

def MerkleTreeParams.idx_to_top (self: MerkleTreeParams) (idx: Nat): Nat := idx - self.top_size

def MerkleTreeParams.idx_to_rest (_self: MerkleTreeParams) (idx: Nat): Nat := idx - 1

namespace Examples

def ex_1: MerkleTreeParams := MerkleTreeParams.new 1024 1234 50

#eval ex_1.layers == 10
#eval ex_1.top_layer == 5
#eval ex_1.top_size == 32

def ex_2: MerkleTreeParams := MerkleTreeParams.new 2048 31337 128

#eval ex_2.layers == 11
#eval ex_2.top_layer == 7
#eval ex_2.top_size == 128

end Examples


structure MerkleTreeVerifier where
  params: MerkleTreeParams
  top: Array Sha256.Digest
  rest: Array Sha256.Digest

def MerkleTreeVerifier.root (self: MerkleTreeVerifier): Sha256.Digest
  := if self.rest.size == 0
      then self.top[MerkleTreeParams.idx_to_top self.params 1]!
      else self.rest[MerkleTreeParams.idx_to_rest self.params 1]!

partial def MerkleTreeVerifier.fillUpperRest (params: MerkleTreeParams) (top out: Array Sha256.Digest) (fr to: Nat) (i: Nat := fr - 1): Array Sha256.Digest
  := if i < to
      then out
      else
        let top_idx := MerkleTreeParams.idx_to_top params (2 * i)
        let out_idx := MerkleTreeParams.idx_to_rest params i
        let out' := Array.set! out out_idx (Hash.hash_pair top[top_idx]! top[top_idx + 1]!)
        MerkleTreeVerifier.fillUpperRest params top out' fr to (i - 1)

partial def MerkleTreeVerifier.fillLowerRest (params: MerkleTreeParams) (out: Array Sha256.Digest) (fr to: Nat) (i: Nat := fr - 1): Array Sha256.Digest
  := if i < to
      then out
      else
        let upper_rest_idx := MerkleTreeParams.idx_to_rest params (2 * i)
        let out_idx := MerkleTreeParams.idx_to_rest params i
        let out' := Array.set! out out_idx (Hash.hash_pair out[upper_rest_idx]! out[upper_rest_idx + 1]!)
        MerkleTreeVerifier.fillLowerRest params out' fr to (i - 1)

def MerkleTreeVerifier.new [Monad M] [MonadReadIop M] (row_size col_size queries: Nat): M MerkleTreeVerifier
  := do let params := MerkleTreeParams.new row_size col_size queries
        let top <- MonadReadIop.readPodSlice Sha256.Digest params.top_size
        let mut rest := Array.mkArray (params.top_size - 1) Inhabited.default
        rest := MerkleTreeVerifier.fillUpperRest params top rest params.top_size (params.top_size / 2)
        rest := MerkleTreeVerifier.fillLowerRest params rest (params.top_size / 2) 1
        let verifier: MerkleTreeVerifier := {
          params,
          top,
          rest
        }
        MonadReadIop.commit (MerkleTreeVerifier.root verifier)
        pure verifier

def MerkleTreeVerifier.verify [Monad M] [MonadReadIop M] [MonadExceptOf VerificationError M] 
  (idx_ : MonadStateOf Nat M) [R0sy.Algebra.Field Elem] 
  (self: MerkleTreeVerifier) : M (Array Elem) := do 
  let mut idx <- idx_.get
  if idx >= 2 * self.params.row_size 
    then throw (VerificationError.MerkleQueryOutOfRange idx self.params.row_size) 
    else
  let out <- MonadReadIop.readFields Elem self.params.col_size
  let mut cur := Hash.hash_pod out
  idx := idx + self.params.row_size
  while idx >= 2 * self.params.row_size do
    let low_bit := idx % 2
    let otherArray <- MonadReadIop.readPodSlice Sha256.Digest 1
    let other := otherArray[0]!
    idx := idx / 2
    if low_bit == 1 
    then 
      cur := Hash.hash_pair other cur
    else 
      cur := Hash.hash_pair cur other
  let present_hash := 
    if idx >= self.params.top_size 
      then self.top[self.params.idx_to_top idx]!
      else self.top[self.params.idx_to_rest idx]!
  idx_.set idx
  if present_hash == cur 
  then
    return out
  else 
    throw VerificationError.InvalidProof

end Zkvm.Verify.Merkle
