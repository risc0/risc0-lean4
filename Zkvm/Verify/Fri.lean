/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/


import R0sy
import Zkvm.ArithVM.Circuit
import Zkvm.Constants
import Zkvm.Verify.Classes
import Zkvm.Verify.Merkle

namespace Zkvm.Verify.Fri

open R0sy.Algebra
open R0sy.Algebra.Ntt
open R0sy.Algebra.Poly
open R0sy.Lean.Subarray
open R0sy.Lean.Nat
open R0sy.Hash.Sha2
open R0sy.Hash
open ArithVM.Circuit
open Classes
open Constants
open Merkle
open Field


structure FriVerifyState (ExtElem: Type) where
  pos: Nat
  goal: ExtElem
  deriving Inhabited

def FriVerifyState.run [Monad M] [Inhabited ExtElem] (f: StateT (FriVerifyState ExtElem) M X): M X
  := StateT.run' f Inhabited.default

def FriVerifyState.get_pos [Monad M]: StateT (FriVerifyState ExtElem) M Nat
  := do let self <- get
        pure self.pos

def FriVerifyState.set_pos [Monad M] (pos: Nat): StateT (FriVerifyState ExtElem) M Unit
  := do let self <- get
        set { self with pos }

def FriVerifyState.get_goal [Monad M]: StateT (FriVerifyState ExtElem) M ExtElem
  := do let self <- get
        pure self.goal

def FriVerifyState.set_goal [Monad M] (goal: ExtElem): StateT (FriVerifyState ExtElem) M Unit
  := do let self <- get
        set { self with goal }


structure VerifyRoundInfo (ExtElem: Type) where
  domain: Nat
  merkle: MerkleTreeVerifier
  mix: ExtElem

def VerifyRoundInfo.new (Elem ExtElem: Type) [Monad M] [MonadReadIop M] [Field ExtElem] [ExtField Elem ExtElem]
  (in_domain: Nat) : M (VerifyRoundInfo ExtElem)
  := do let domain := in_domain / FRI_FOLD
        let merkle <- MerkleTreeVerifier.new domain (FRI_FOLD * ExtField.EXT_DEG Elem ExtElem) QUERIES
        let mix : ExtElem <- Field.random
        return VerifyRoundInfo.mk domain merkle mix

def fold_eval [Field ExtElem] [RootsOfUnity ExtElem] (io : Array ExtElem) (x : ExtElem) : ExtElem
  := Poly.eval (Poly.ofArray (bit_reverse (interpolate_ntt io))) x

-- Takes a array of `T`s of length (outersize * inner_size) 
-- and returns a list of outer_size lists of `T`s, each sublist having inner_size elements
-- Adjacent elems do not go in the same sublist
def collate [Inhabited T] (arr : Array T) (outer_size inner_size : Nat) : (Array (Array T))
  := ((List.range outer_size).map (λ i => ((List.range inner_size).map (λ j => arr[outer_size * j + i]!)).toArray)).toArray
  -- (arr.toList.toChunks outer_size).transpose.toArray -- Uses Std

def VerifyRoundInfo.verify_query (Elem ExtElem: Type) [Monad M] [MonadReadIop M] [MonadExceptOf VerificationError M] [Algebraic Elem ExtElem]
  (self: VerifyRoundInfo ExtElem) : StateT (FriVerifyState ExtElem) M Unit
  := do let pos <- FriVerifyState.get_pos
        let goal <- FriVerifyState.get_goal
        --
        let quot := pos / self.domain
        let group := pos % self.domain
        let data : Array Elem <- MonadLift.monadLift (self.merkle.verify group: M (Array Elem))
        -- collect field elements into groups of size EXT_SIZE
        let collate_data : Array (Array Elem) := collate data FRI_FOLD (ExtField.EXT_DEG Elem ExtElem)
        let data_ext : Array ExtElem := collate_data.map ExtField.ofSubelems 
        if data_ext[quot]! != goal then throw VerificationError.InvalidProof
        let root_po2 : Nat := Nat.log2_ceil (FRI_FOLD * self.domain)
        let inv_wk : Elem := (RootsOfUnity.ROU_REV[root_po2]! : Elem) ^ group
        -- Track the states of the mutable arguments
        FriVerifyState.set_pos group
        let new_goal := fold_eval data_ext (self.mix * inv_wk)
        FriVerifyState.set_goal new_goal
        pure ()


def fri_verify [Monad M] [MonadReadIop M] [MonadExceptOf VerificationError M] [Algebraic Elem ExtElem]
  (degree : Nat) (inner : Nat -> M ExtElem) : M Unit
  := do let mut degree_ := degree
        let orig_domain := INV_RATE * degree
        let mut domain := orig_domain
        -- Prep the folding verfiers
        let rounds_capacity := ((Nat.log2_ceil ((degree + FRI_FOLD - 1) / FRI_FOLD)) + FRI_FOLD_PO2 - 1) / FRI_FOLD_PO2 -- this is just for performance in the rust
        let mut rounds : Array (VerifyRoundInfo ExtElem) := Array.mkEmpty rounds_capacity
        while degree > FRI_MIN_DEGREE do
          let round <- VerifyRoundInfo.new Elem ExtElem domain
          rounds := rounds.push round
          domain := domain / FRI_FOLD
          degree_ := degree_ / FRI_FOLD
        let final_coeffs : Array Elem <- MonadReadIop.readFields Elem (ExtField.EXT_DEG Elem ExtElem * degree)
        let final_digest := Hash.hash_pod (final_coeffs)
        MonadReadIop.commit final_digest
        -- // Get the generator for the final polynomial evaluations
        let gen : Elem := RootsOfUnity.ROU_FWD[Nat.log2_ceil (domain)]!
        -- // Do queries
        FriVerifyState.run do
          let mut poly_buf: Array ExtElem := Array.mkEmpty degree_
          for _ in [0:QUERIES] do
            let rng: UInt32 <- MonadLift.monadLift (MonadRng.nextUInt32: M UInt32)
            let pos_val := ((rng.toNat) % orig_domain)
            FriVerifyState.set_pos pos_val
            -- // Do the 'inner' verification for this index
            let eval_inner : ExtElem <- inner pos_val 
            FriVerifyState.set_goal eval_inner --inner(iop, pos)?
            -- // Verify the per-round proofs
            for round in rounds do
              VerifyRoundInfo.verify_query Elem ExtElem round
            -- // Do final verification
            let x : Elem := gen ^ (<- FriVerifyState.get_pos)
            -- collect field elements into groups of size EXT_SIZE
            let collate_final_coeffs : Array (Array Elem) := collate final_coeffs degree_ (ExtField.EXT_DEG Elem ExtElem)
            poly_buf := collate_final_coeffs.map ExtField.ofSubelems 

            let fx : ExtElem := Poly.eval (Poly.ofArray poly_buf) (Algebra.ofBase x)

            if fx != (<- FriVerifyState.get_goal) then throw VerificationError.InvalidProof
        return ()


end Zkvm.Verify.Fri
