/-
 Copyright 2023 RISC Zero, Inc.

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
-/

import Zkvm
import R0sy.Hash
import Std.Data.Rat.Basic
import Std.Data.List.Basic

namespace Soundness.ProofSystem

open R0sy.Hash

/-- A Type of all possible things that could be hashed -/
def HashPreimage (D : Type) := ByteArray ⊕ Subarray UInt32 ⊕ (D × D) ⊕ Array (Array UInt32)

/-- A function to evaluate the hash on an arbitrary Hash-able type -/
def HashEval (h : Hash D): (input : HashPreimage D) -> D 
| Sum.inl x => h.hash x
| Sum.inr (Sum.inl x) => h.hash_words x
| Sum.inr (Sum.inr (Sum.inl ⟨x, y⟩)) => h.hash_pair x y
| Sum.inr (Sum.inr (Sum.inr x)) => h.hash_array_array x

/-- Given a hash function and an adversary choosing a next query from a list of previous oracle responses, return the list of all responses -/
def query_list {D : Type} :
  Nat -> (Hash D) -> (List D -> HashPreimage D) -> List D 
| 0, _, _ => List.nil
| (Nat.succ n), ro, next => (HashEval ro (next (query_list n ro next))) :: (query_list n ro next)

/-- Structure for an agent which works by querying a hashing scheme several times and then returning a value based on the output of the hashes -/
structure query_bounded_adversary (D Out : Type) :=
  (adversary_query_generator : List D -> HashPreimage D)
  (adversary_out_from_list : List D -> Out) 

/-- Returns the output of the query bounded adversary, with a particular query bound and hashing scheme -/
def query_bounded_adversary.to_fun {D Out : Type} 
  (𝓐 : query_bounded_adversary D Out) (query_count : Nat) (hash_function : Hash D) :
  Out :=
𝓐.adversary_out_from_list (query_list query_count hash_function (𝓐.adversary_query_generator))

/-- A monadic type constructor for representing Probability mass functions -/
def Pmf α := List α

instance : Monad Pmf where
  pure := List.pure
  bind := List.bind

def Pmf.prod (a : Pmf α) (b : Pmf β) : Pmf (α × β) := do
  let a_ <- a
  let b_ <- b
  return ⟨a_, b_⟩

def Pmf.prob [BEq α] (p : Pmf α) (a : α) : Rat := List.count a p / List.length p

/-- Given a list of items of type α and a Pmf over β, returns a Pmf over functions α -> β, 
sending each α in the list independently to the Pmf, and all other α to the default -/
def Pmf.func [BEq α] (as : List α) (default : β) (bs : Pmf β) : Pmf (α -> β) := 
match as with
| [] => [λ _ => default]
| head :: tail => do
  let rec <- Pmf.func tail default bs
  let b <- bs
  return λ a => if a == head then b else rec a

/-- 
Defines the soundness criterion: there must exist an extractor, such that for any adversary making a certain number of queries and trying to convince the verifier of a particular statement, if the adversary is likely to succeed, then we can extract a witness from the adversary. 
-/
def verifier_soundness {Stmt Wit D Proof : Type} (relation : Stmt -> Wit -> Prop) (random_oracles : Pmf (Hash D))   
  (verifier : [Hash D] -> Stmt -> Proof -> Bool) (soundness_bound : Nat -> Rat) : Prop :=
  ∃ (knowledge_extractor : (query_bounded_adversary D Proof) -> Wit),
    -- adversary query generator takes a input and outputs a next query
    ∀ (query_count : Nat), ∀ (stmt : Stmt), ∀ (𝓐 : query_bounded_adversary D Proof),
      -- If the adversary has probability greater than the soundness bound of convincing the verifier ...
      (let adv_verifies : Pmf Bool := (do
        let (h : Hash D) <- random_oracles
        return (@verifier h stmt (@query_bounded_adversary.to_fun _ _ 𝓐 query_count h))) 
      Pmf.prob adv_verifies true ≥ soundness_bound query_count)
        -- ... then the extractor obtains a correct witness.
        -> relation stmt (knowledge_extractor 𝓐)

/--
A structure for a non-interactive proof system in the random oracle model.
This could be modified to use a hybrid random-oracle/collision-resistant-hash-function model by adding more types for the input and output of that function.
-/
structure noninteractive_random_oracle_proof_scheme :=    -- n_ro_codomain= 2^256 usually
  (Stmt : Type)
  (Wit : Type)
  (D : Type)
  (relation : Stmt -> Wit → Prop)
  (random_oracles : Pmf (Hash D)) -- A pmf uniform over all random oracles
  (Proof : Type)
  (prover :
    [Hash D] -> Stmt -> Wit -> Proof
  )
  (verifier :
    [Hash D] -> Stmt -> Proof -> Bool
  )
  (completeness : -- Perfect completeness here
    ∀ (ro : Hash D), ∀ (stmt : Stmt), ∀ (wit : Wit),
      relation stmt wit -> @verifier ro stmt (@prover ro stmt wit)
  )
  -- Given n queries to oracle, what is worst case probability of compromise?
  (soundness_bound : Nat -> Rat)
  (soundness : verifier_soundness relation random_oracles verifier soundness_bound )


end Soundness.ProofSystem


/-- A verifier function, exactly analogous to that in check_seal, 
but defined as a pure function with the type signature needed to pass into the soundness proposition -/
def verifier [h : R0sy.Hash.Hash R0sy.Hash.Sha2.Sha256.Digest] 
  (stmt : Zkvm.ArithVM.Circuit.Circuit × Zkvm.MethodId.MethodId R0sy.Hash.Sha2.Sha256.Digest × Array UInt32)
  (seal: Array UInt32) : Bool :=
let ⟨circuit, method_id, journal⟩ := stmt
let result := Zkvm.Verify.ReadIop.ReadIop.run seal (@Zkvm.Verify.verify _ h circuit method_id journal)
match result with
| Except.ok _ => True
| Except.error _ => False

def soundness_bound (queries : Nat) : Rat := queries / ((2 ^ 128 : Nat) : Rat)

instance : BEq ByteArray := 
{ beq := λ a b => a.data == b.data }

instance : BEq (Subarray UInt32) := 
{ beq := λ a b => a.toArray == b.toArray }

/-- A list of all byte arrays that can be hashed -/
def used_byte_arrays : List ByteArray := sorry

/-- A list of all byte arrays that can be hashed -/
def used_subarrays : List (Subarray UInt32) := sorry

/-- A list of all byte arrays that can be hashed -/
def used_array_arrays : List (Array (Array UInt32)) := sorry

/-- All digests -/
def all_digests : List (R0sy.Hash.Sha2.Sha256.Digest) := sorry

/-- All digest pairs -/
def all_digest_pairs : List (R0sy.Hash.Sha2.Sha256.Digest × R0sy.Hash.Sha2.Sha256.Digest) := sorry

def random_oracles : Soundness.ProofSystem.Pmf (R0sy.Hash.Hash R0sy.Hash.Sha2.Sha256.Digest) := do
  let byte_array_maps <- Soundness.ProofSystem.Pmf.func used_byte_arrays default all_digests
  let subarray_maps <- Soundness.ProofSystem.Pmf.func used_subarrays default all_digests
  let pair_maps : R0sy.Hash.Sha2.Sha256.Digest × R0sy.Hash.Sha2.Sha256.Digest → R0sy.Hash.Sha2.Sha256.Digest <- Soundness.ProofSystem.Pmf.func all_digest_pairs default all_digests
  let curried_pair_maps := λ x y => pair_maps ⟨x, y⟩
  let array_array_maps <- Soundness.ProofSystem.Pmf.func used_array_arrays default all_digests
  return R0sy.Hash.Hash.mk byte_array_maps subarray_maps curried_pair_maps array_array_maps


def relation (stmt : Zkvm.ArithVM.Circuit.Circuit × Zkvm.MethodId.MethodId R0sy.Hash.Sha2.Sha256.Digest × Array UInt32) (wit : Nat) : Prop := sorry 

def main_soundness_claim : Soundness.ProofSystem.verifier_soundness relation random_oracles verifier soundness_bound := 
by
  unfold Soundness.ProofSystem.verifier_soundness
  sorry