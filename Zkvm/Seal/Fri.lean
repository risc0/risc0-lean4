/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/


import R0sy
import Zkvm.ArithVM.Circuit
import Zkvm.Constants
import Zkvm.Verify.Classes
import Zkvm.Verify.Merkle
import Zkvm.Verify.Monad

namespace Zkvm.Seal.Fri

open R0sy.Algebra
open R0sy.Algebra.Ntt
open R0sy.Algebra.Poly
open R0sy.Lean.Subarray
open R0sy.Lean.Nat
open R0sy.Hash.Sha2
open R0sy.Hash
open ArithVM.Circuit
open Constants
open Field
open Verify.Classes
open Verify.Merkle
open Verify.Monad


-- Takes a array of `T`s of length (outersize * inner_size) 
-- and returns a list of outer_size lists of `T`s, each sublist having inner_size elements
-- Adjacent elems do not go in the same sublist
def collate [Inhabited T] (arr : Array T) (outer_size inner_size : Nat) : (Array (Array T))
  := ((List.range outer_size).map (λ i => ((List.range inner_size).map (λ j => arr[outer_size * j + i]!)).toArray)).toArray
  -- (arr.toList.toChunks outer_size).transpose.toArray -- Uses Std


structure FriState (ExtElem: Type) where
  pos: Nat
  goal: ExtElem
  deriving Inhabited

namespace FriState
  def run [Monad M] [Inhabited ExtElem] (f: StateT (FriState ExtElem) M X): M X
    := StateT.run' f Inhabited.default

  def get_pos [Monad M]: StateT (FriState ExtElem) M Nat
    := do let self <- get
          pure self.pos

  def set_pos [Monad M] (pos: Nat): StateT (FriState ExtElem) M Unit
    := do let self <- get
          set { self with pos }

  def get_goal [Monad M]: StateT (FriState ExtElem) M ExtElem
    := do let self <- get
          pure self.goal

  def set_goal [Monad M] (goal: ExtElem): StateT (FriState ExtElem) M Unit
    := do let self <- get
          set { self with goal }
end FriState


structure FriRoundVerifier (ExtElem: Type) where
  domain: Nat
  merkle: MerkleTreeVerifier
  mix: ExtElem

namespace FriRoundVerifier
  def read_and_commit (Elem ExtElem: Type) [Monad M] [MonadReadIop M] [Field ExtElem] [ExtField Elem ExtElem]
    (in_domain: Nat) : M (FriRoundVerifier ExtElem)
    := do let domain := in_domain / FRI_FOLD
          let merkle <- MerkleTreeVerifier.read_and_commit domain (FRI_FOLD * ExtField.EXT_DEG Elem ExtElem) QUERIES
          let mix : ExtElem <- Field.random
          pure {
            domain,
            merkle,
            mix
          }

  def verify (Elem ExtElem: Type) [Monad M] [MonadReadIop M] [MonadExceptOf VerificationError M] [Algebraic Elem ExtElem] (self: FriRoundVerifier ExtElem) : StateT (FriState ExtElem) M Unit
    := do let pos <- FriState.get_pos
          let goal <- FriState.get_goal
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
          FriState.set_pos group
          let new_goal := Poly.eval (Poly.ofArray (bit_reverse (interpolate_ntt data_ext))) (self.mix * inv_wk)
          FriState.set_goal new_goal
          pure ()
end FriRoundVerifier


structure FriVerifier (Elem ExtElem: Type) where
  orig_domain: Nat
  domain: Nat
  rounds: Array (FriRoundVerifier ExtElem)
  final_coeffs: Array Elem
  poly: Poly ExtElem

def read_and_commit (Elem ExtElem: Type) [MonadVerify M] [Algebraic Elem ExtElem] (in_degree : Nat): M (FriVerifier Elem ExtElem)
  := do let mut degree := in_degree
        let orig_domain := INV_RATE * in_degree
        let mut domain := orig_domain
        -- Prep the folding verfiers
        let rounds_capacity := ((Nat.log2_ceil ((in_degree + FRI_FOLD - 1) / FRI_FOLD)) + FRI_FOLD_PO2 - 1) / FRI_FOLD_PO2 -- this is just for performance in the rust
        let mut rounds := Array.mkEmpty rounds_capacity
        while degree > FRI_MIN_DEGREE do
          let round <- FriRoundVerifier.read_and_commit Elem ExtElem domain
          rounds := rounds.push round
          domain := domain / FRI_FOLD
          degree := degree / FRI_FOLD
        let final_coeffs : Array Elem <- MonadReadIop.readFields Elem (ExtField.EXT_DEG Elem ExtElem * degree)
        let collate_final_coeffs : Array (Array Elem) := collate final_coeffs degree (ExtField.EXT_DEG Elem ExtElem)
        let poly: Poly ExtElem := Poly.ofArray (collate_final_coeffs.map ExtField.ofSubelems)
        MonadReadIop.commit (Hash.hash_pod final_coeffs)
        pure {
          orig_domain,
          domain,
          rounds,
          final_coeffs,
          poly
        }

def verify [MonadVerify M] [Algebraic Elem ExtElem] (fri_verify_params: FriVerifier Elem ExtElem) (inner : Nat -> M ExtElem) : M Unit
  := do -- // Get the generator for the final polynomial evaluations
        let gen : Elem := RootsOfUnity.ROU_FWD[Nat.log2_ceil (fri_verify_params.domain)]!
        -- // Do queries
        FriState.run do
          for query_no in [0:QUERIES] do
            let rng: UInt32 <- MonadLift.monadLift (MonadRng.nextUInt32: M UInt32)
            let pos_val := rng.toNat % fri_verify_params.orig_domain
            FriState.set_pos pos_val
            inner pos_val >>= FriState.set_goal
            -- // Verify the per-round proofs
            for round in fri_verify_params.rounds do
              FriRoundVerifier.verify Elem ExtElem round
            -- // Do final verification
            let x : Elem := gen ^ (<- FriState.get_pos)
            let goal <- FriState.get_goal
            let actual : ExtElem := Poly.eval fri_verify_params.poly (Algebra.ofBase x)
            if actual != goal then throw (VerificationError.FriGoalMismatch query_no s!"{goal}" s!"{actual}")
        return ()

end Zkvm.Seal.Fri
