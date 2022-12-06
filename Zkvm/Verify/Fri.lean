/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import R0sy.Algebra.Field
import R0sy.Lean.Subarray
import R0sy.Lean.Nat
import R0sy.Hash.Sha2
import Zkvm.ArithVM.Circuit
import Zkvm.Constants
import Zkvm.Verify.Classes
import Zkvm.Verify.Merkle

namespace Zkvm.Verify.Fri

open R0sy.Algebra
open R0sy.Lean.Subarray
open R0sy.Lean.Nat
open R0sy.Hash.Sha2
open R0sy.Hash
open Classes
open Constants
open Merkle
-- open Field

-- The structure needs to be given the type of the field it is proving over, lmk if there's a more elegant way for this.
structure VerifyRoundInfo (ExtElem: Type) where
  domain: Nat
  merkle: MerkleTreeVerifier
  mix: ExtElem

def VerifyRoundInfo.new [Monad M] [MonadReadIop M] [MonadRng M] 
  [R0sy.Algebra.Field ExtElem] [e : R0sy.Algebra.Algebra Elem ExtElem]
  (in_domain: Nat) : M (VerifyRoundInfo ExtElem) := do
  let domain := in_domain / FRI_FOLD
  let merkle <- MerkleTreeVerifier.new domain (FRI_FOLD * e.EXT_SIZE) QUERIES
  let mix : ExtElem <- Field.random
  return VerifyRoundInfo.mk domain merkle mix

def fold_eval (io : Array ExtElem) (x : ExtElem) : ExtElem := sorry

def poly_eval (coeffs : Array ExtElem) (x : ExtElem) : ExtElem := sorry

def from_subelems (inps : Array Elem) : ExtElem := sorry

-- Takes a array of `T`s of length (outersize * inner_size) 
-- and returns a list of outer_size lists of `T`s, each sublist having inner_size elements
-- Adjacent elems do not go in the same sublist
def collate (arr : Array T) (outer_size : Nat) : (Array (Array T)) := sorry
  -- (arr.toList.toChunks outer_size).transpose.toArray

def VerifyRoundInfo.verify_query [Monad M] [MonadReadIop M] [MonadExceptOf VerificationError M] 
  [RootsOfUnity Elem]
  (pos : MonadStateOf Nat M) (goal : MonadStateOf ExtElem M) -- Weird to do this with typeclasses, what if we had two mutable Nats for example?
  [R0sy.Algebra.Field Elem] [R0sy.Algebra.Field ExtElem] [R0sy.Algebra.Algebra Elem ExtElem]
  (self: VerifyRoundInfo ExtElem) : M Unit := do
  -- Get the args out of the monad state
  let pos_ : Nat <- pos.get 
  let goal_ : ExtElem <- goal.get
  --
  let quot := pos_ / self.domain
  let group := pos_ % self.domain
  let data : Array Elem <- self.merkle.verify group
  let collate_data : Array (Array Elem) := collate data FRI_FOLD -- collect field elements into groups of size EXT_SIZE
  let data_ext : Array ExtElem := collate_data.map from_subelems 
  if data_ext[quot]! != goal_
    then throw VerificationError.InvalidProof
  else 
  let root_po2 : Nat := Nat.log2_ceil (FRI_FOLD * self.domain)
  -- FIXME type of inv_wk should be Elem, and self.mix * inv_wk should be HMul
  let inv_wk : Elem := (RootsOfUnity.ROU_REV[root_po2]! : Elem) ^ group -- Weird why type ascription needed?
  -- Track the states of the mutable arguments
  pos.set group 
  let new_goal := fold_eval data_ext (self.mix * inv_wk)
  goal.set new_goal
  pure ()

def fri_verify [Monad M] [MonadReadIop M] [MonadExceptOf VerificationError M] [rng : MonadRng M]
  [RootsOfUnity Elem]
  (pos : MonadStateOf Nat M) (goal : MonadStateOf ExtElem M)
  [R0sy.Algebra.Field Elem] [R0sy.Algebra.Field ExtElem] [e: R0sy.Algebra.Algebra Elem ExtElem]
  (degree : Nat) (inner : Monad M -> Nat -> ExtElem) : M Unit := do
    let mut degree_ := degree
    let orig_domain := INV_RATE * degree
    let mut domain := orig_domain
    -- Prep the folding verfiers
    let rounds_capacity := ((Nat.log2_ceil ((degree + FRI_FOLD - 1) / FRI_FOLD)) + FRI_FOLD_PO2 - 1) / FRI_FOLD_PO2 -- this is just for performance in the rust
    let mut rounds : Array (VerifyRoundInfo ExtElem) := Array.mkEmpty rounds_capacity
    while degree > FRI_MIN_DEGREE do
      let round <- @VerifyRoundInfo.new _ ExtElem Elem _ _ _ _ _ domain
      rounds := rounds.push round
      domain := domain / FRI_FOLD
      degree_ := degree_ / FRI_FOLD

    -- // We want to minimize reallocation in verify, so make sure we
    -- // didn't have to reallocate.
    -- assert!(
    --     rounds.len() < rounds_capacity,
    --     "Did not allocate enough rounds; needed {} for degree {} but only allocated {}",
    --     rounds.len(),
    --     degree,
    --     rounds_capacity
    -- );
    -- // Grab the final coeffs + commit
    let final_coeffs : Array Elem <- MonadReadIop.readFields Elem (e.EXT_SIZE * degree)
    let final_digest := Hash.hash_pod (final_coeffs)
    MonadReadIop.commit final_digest

    -- // Get the generator for the final polynomial evaluations
    let gen : Elem := RootsOfUnity.ROU_FWD[Nat.log2_ceil (domain)]!
    -- // Do queries
    let mut poly_buf: Array ExtElem := Array.mkEmpty degree_
    for _ in [0:QUERIES] do
      let rng: UInt32 <- @MonadRng.nextUInt32 _ rng
      pos.set ((rng.toNat) % orig_domain)
      -- // Do the 'inner' verification for this index
      goal.set sorry --inner(iop, pos)?
      -- // Verify the per-round proofs
      for round in rounds do
        @VerifyRoundInfo.verify_query _ Elem ExtElem _ _ _ _ pos goal _ _ _ round
      
      -- // Do final verification
      let x : Elem := gen ^ (<- pos.get)
      -- collect field elements into groups of size EXT_SIZE
      let collate_final_coeffs : Array (Array Elem) := collate final_coeffs degree_
      poly_buf := collate_final_coeffs.map from_subelems

      let fx : ExtElem := poly_eval poly_buf (e.ofBase x)
      if fx != (<- goal.get)
        then
          throw VerificationError.InvalidProof -- XXX surprised this is valid syntax (without the "else"), I assume it does what it looks like it does?
    return ()


end Zkvm.Verify.Fri
