/-
Copyright (c) 2023 RISC Zero. All rights reserved.
-/

import R0sy.Hash
import Std.Data.Rat.Basic
import Std.Data.List.Basic

open R0sy.Hash

/-- A Type of all possible things that could be hashed -/
def HashPreimage (D : Type) := ByteArray ⊕ Subarray UInt32 ⊕ (D × D) ⊕ Array (Array UInt32)

def HashEval (h : Hash D): (input : HashPreimage D) -> D 
| Sum.inl x => h.hash x
| Sum.inr (Sum.inl x) => h.hash_words x
| Sum.inr (Sum.inr (Sum.inl ⟨x, y⟩)) => h.hash_pair x y
| Sum.inr (Sum.inr (Sum.inr x)) => h.hash_array_array x

-- given a ro and an adversary choosing a next query from a list of previous oracle responses,
-- return the list of all responses
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

def Pmf α := List α

instance : Monad Pmf where
  pure := List.pure
  bind := List.bind

-- TODO
def Pmf.prob [BEq α] (p : Pmf α) (a : α) : Rat := List.count a p / List.length p

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
  -- Given an adversarial query generator and way of generating a proof from the queries, the extractor outputs the witness the generator knows
  (knowledge_extractor : (query_bounded_adversary D Proof) -> Wit)
  (soundness :
    ∀ (query_count : Nat), ∀ (stmt : Stmt),
      -- adversary query generator takes a input and outputs a next query
      ∀ (𝓐 : query_bounded_adversary D Proof),
        -- If the adversary has probability greater than the soundness bound of convincing the verifier ...
        (let adv_verifies : Pmf Bool := (do
          let (h : Hash D) <- random_oracles -- why is this a type mismatch?
          -- let h : Hash D := sorry
          return (@verifier h stmt (@query_bounded_adversary.to_fun _ _ 𝓐 query_count h))) 
        Pmf.prob adv_verifies true ≥ soundness_bound query_count)
          -- ... then the extractor obtains a correct witness.
          -> relation stmt (knowledge_extractor 𝓐))


