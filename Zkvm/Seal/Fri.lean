/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/


import R0sy
import Zkvm.Algebra.Classes
import Zkvm.Algebra.Ntt
import Zkvm.ArithVM.Circuit
import Zkvm.Constants
import Zkvm.Verify.Error
import Zkvm.Verify.Merkle
import Zkvm.Verify.ReadIop

namespace Zkvm.Seal.Fri

open R0sy.Lean.Subarray
open R0sy.Lean.Nat
open R0sy.Hash
open Zkvm.Algebra.Classes
open Zkvm.Algebra.Ntt
open Zkvm.ArithVM.Circuit
open Zkvm.Constants
open Zkvm.Verify.Error
open Zkvm.Verify.Merkle
open Zkvm.Verify.ReadIop


-- Takes a array of `T`s of length (outersize * inner_size) 
-- and returns a list of outer_size lists of `T`s, each sublist having inner_size elements
-- Adjacent elems do not go in the same sublist
def collate [Inhabited T] (arr : Array T) (outer_size inner_size : Nat) : (Array (Array T))
  := ((List.range outer_size).map (λ i => ((List.range inner_size).map (λ j => arr[outer_size * j + i]!)).toArray)).toArray
  -- (arr.toList.toChunks outer_size).transpose.toArray -- Uses Std


structure FriGoalState (ExtElem: Type) where
  pos: Nat
  goal: ExtElem
  deriving Inhabited

namespace FriGoalState
  def run [Monad M] [Inhabited ExtElem] (f: StateT (FriGoalState ExtElem) M X): M X
    := StateT.run' f Inhabited.default

  def get_pos [Monad M]: StateT (FriGoalState ExtElem) M Nat
    := do let self <- get
          pure self.pos

  def set_pos [Monad M] (pos: Nat): StateT (FriGoalState ExtElem) M Unit
    := do let self <- get
          set { self with pos }

  def get_goal [Monad M]: StateT (FriGoalState ExtElem) M ExtElem
    := do let self <- get
          pure self.goal

  def set_goal [Monad M] (goal: ExtElem): StateT (FriGoalState ExtElem) M Unit
    := do let self <- get
          set { self with goal }
end FriGoalState


structure FriRoundVerifier (D ExtElem: Type) where
  domain: Nat
  merkle: MerkleTreeVerifier D
  mix: ExtElem

namespace FriRoundVerifier
  def read_and_commit [Monad M] [MonadReadIop M] [MonadCommitIop D M] [Hash D] (circuit: Circuit) (in_domain: Nat) : M (FriRoundVerifier D circuit.field.ExtElem)
    := do let domain := in_domain / FRI_FOLD
          let merkle <- MerkleTreeVerifier.read_and_commit domain (FRI_FOLD * ExtField.EXT_DEG circuit.field.Elem circuit.field.ExtElem) QUERIES
          let mix : circuit.field.ExtElem <- Field.random
          pure {
            domain,
            merkle,
            mix
          }

  def verify (Elem ExtElem: Type) [Monad M] [MonadReadIop M] [Hash D] [MonadExceptOf VerificationError M] [Algebraic Elem ExtElem] (self: FriRoundVerifier D ExtElem) : StateT (FriGoalState ExtElem) M Unit
    := do let pos <- FriGoalState.get_pos
          let goal <- FriGoalState.get_goal
          --
          let quot := pos / self.domain
          let group := pos % self.domain
          let data : Array Elem <- MonadLift.monadLift (self.merkle.verify group: M (Array Elem))
          -- collect field elements into groups of size EXT_SIZE
          let collate_data : Array (Array Elem) := collate data FRI_FOLD (ExtField.EXT_DEG Elem ExtElem)
          let data_ext : Array ExtElem := collate_data.map ExtField.ofSubelems 
          -- Returns error if there's a mismatch from one round of FRI to the next
          if data_ext[quot]! != goal then throw VerificationError.FRICommitRoundMismatch
          let root_po2 : Nat := Nat.log2_ceil (FRI_FOLD * self.domain)
          let inv_wk : Elem := (RootsOfUnity.ROU_REV[root_po2]! : Elem) ^ group
          -- Track the states of the mutable arguments
          FriGoalState.set_pos group
          let new_goal := polyEval (bit_reverse (interpolate_ntt data_ext)).toSubarray (self.mix * inv_wk)
          FriGoalState.set_goal new_goal
          pure ()
end FriRoundVerifier


structure FriVerifier (D Elem ExtElem: Type) where
  orig_domain: Nat
  domain: Nat
  rounds: Array (FriRoundVerifier D ExtElem)
  final_coeffs: Array Elem
  poly: Subarray ExtElem

def read_and_commit
    (D: Type)
    [Monad M]
    [MonadReadIop M]
    [MonadCommitIop D M]
    [MonadExceptOf VerificationError M]
    [Hash D]
    (circuit: Circuit) (in_degree : Nat): M (FriVerifier D circuit.field.Elem circuit.field.ExtElem)
  := do let mut degree := in_degree
        let orig_domain := INV_RATE * in_degree
        let mut domain := orig_domain
        -- Prep the folding verfiers
        let rounds_capacity := ((Nat.log2_ceil ((in_degree + FRI_FOLD - 1) / FRI_FOLD)) + FRI_FOLD_PO2 - 1) / FRI_FOLD_PO2 -- this is just for performance in the rust
        let mut rounds := Array.mkEmpty rounds_capacity
        while degree > FRI_MIN_DEGREE do
          let round <- FriRoundVerifier.read_and_commit circuit domain
          rounds := rounds.push round
          domain := domain / FRI_FOLD
          degree := degree / FRI_FOLD
        let final_coeffs <- MonadReadIop.readFields circuit.field.Elem (ExtField.EXT_DEG circuit.field.Elem circuit.field.ExtElem * degree)
        let collate_final_coeffs := collate final_coeffs degree (ExtField.EXT_DEG circuit.field.Elem circuit.field.ExtElem)
        let poly: Subarray circuit.field.ExtElem := (collate_final_coeffs.map ExtField.ofSubelems).toSubarray
        let h_coeffs: D := Hash.hash_pod final_coeffs
        MonadCommitIop.commit h_coeffs
        pure {
          orig_domain,
          domain,
          rounds,
          final_coeffs,
          poly
        }

def verify [Monad M] [MonadReadIop M] [MonadExceptOf VerificationError M] [Hash D] [Algebraic Elem ExtElem] (fri_verify_params: FriVerifier D Elem ExtElem) (inner : Nat -> M ExtElem) : M Unit
  := do -- // Get the generator for the final polynomial evaluations
        let gen : Elem := RootsOfUnity.ROU_FWD[Nat.log2_ceil (fri_verify_params.domain)]!
        -- // Do queries
        FriGoalState.run do
          for query_no in [0:QUERIES] do
            let rng: UInt32 <- MonadLift.monadLift (MonadRng.nextUInt32: M UInt32)
            let pos_val := rng.toNat % fri_verify_params.orig_domain
            FriGoalState.set_pos pos_val
            inner pos_val >>= FriGoalState.set_goal
            -- // Verify the per-round proofs
            for round in fri_verify_params.rounds do
              FriRoundVerifier.verify Elem ExtElem round
            -- // Do final verification
            let x : Elem := gen ^ (<- FriGoalState.get_pos)
            -- TODO clarify logic
            let goal <- FriGoalState.get_goal
            -- TODO clarify logic
            let actual : ExtElem := polyEval fri_verify_params.poly (Algebra.ofBase x)
            -- Returns an error if there's a mismatch between the verifier-computed evaluation of the FRI polynomial and the FRI query on the seal
            if actual != goal then throw (VerificationError.FriGoalMismatch query_no s!"{goal}" s!"{actual}")
        return ()

end Zkvm.Seal.Fri
