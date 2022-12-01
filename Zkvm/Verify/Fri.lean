/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import R0sy.Algebra.Field
import R0sy.Lean.Subarray
import R0sy.Lean.Nat
import R0sy.Hash.Sha2
import Zkvm.Circuit
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

def VerifyRoundInfo.verify_query [Monad M] [MonadReadIop M] [MonadExceptOf VerificationError M] 
  [RootsOfUnity Elem] -- FIXME should be Elem
  (pos : MonadStateOf Nat M) (goal : MonadStateOf ExtElem M) -- Weird to do this with typeclasses, what if we had two mutable Nats for example?
  [R0sy.Algebra.Field Elem] [R0sy.Algebra.Field ExtElem] [R0sy.Algebra.Algebra Elem ExtElem]
  (self: VerifyRoundInfo ExtElem) : M Unit := do
  -- Get the args out of the monad state
  let pos_ : Nat <- pos.get 
  let goal_ : ExtElem <- goal.get
  --
  let quot := pos_ / self.domain
  let group := pos_ % self.domain
  let data : Array ExtElem <- self.merkle.verify group
  let data_ext : Array ExtElem := sorry -- Get the column data, not sure yet what the rust is doing here
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


end Zkvm.Verify.Fri
