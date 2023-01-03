/-
Copyright (c) 2023 RISC Zero. All rights reserved.
-/

import R0sy.Hash

open R0sy.Hash


-- given a ro and an adversary choosing a next query from a list of previous oracle responses,
-- return the list of all responses
def query_list {ro_domain ro_codomain : Type} :
  Nat -> (ro_domain -> ro_codomain) -> (List ro_codomain -> ro_domain) -> List ro_codomain 
| 0, _, _ => List.nil
| (Nat.succ n), ro, next => (ro (next (query_list n ro next))) :: (query_list n ro next)

/-- Structure for an agent which works by querying a hashing scheme several times and then returning a value based on the output of the hashes -/
structure query_bounded_adversary (ro_domain ro_codomain Out : Type) :=
  (adversary_query_generator : List (ro_codomain) -> ro_domain)
  (adversary_out_from_list : List (ro_codomain) -> Out) 

/-- Returns the output of the query bounded adversary, with a particular query bound and hashing scheme -/
def query_bounded_adversary.to_fun {D Out : Type} 
  (𝓐 : query_bounded_adversary ro_domain ro_codomain Out) (query_count : Nat) (hash_function : ro_domain -> ro_codomain) :
  Out :=
𝓐.adversary_out_from_list (query_list query_count hash_function (𝓐.adversary_query_generator))

/--
A structure for a non-interactive proof system in the random oracle model.
This could be modified to use a hybrid random-oracle/collision-resistant-hash-function model by adding more types for the input and output of that function.
-/
structure noninteractive_random_oracle_proof_scheme :=    -- n_ro_codomain= 2^256 usually
  (Stmt : Type)
  (Wit : Type)
  (relation : Stmt -> Wit → Prop)
  -- (ro_domain : Type)
  -- [nonempty_ro : nonempty (ro_domain -> fin (n_ro_codomain))] -- technically needed
  -- [fintype_ro : fintype (ro_domain -> fin (n_ro_codomain))] -- technically needed
  (Proof : Type)
  (prover :
    [Hash D] -> Stmt -> Wit -> Proof
  )
  (verifier :
    [Hash D] -> Stmt -> Proof -> Bool
  )
  (completeness : -- Perfect completeness here
    ∀ (ro : Hash D), ∀ (stmt : Stmt), ∀ (wit : Wit),
      relation stmt wit -> @verifier D ro stmt (@prover D ro stmt wit)
  )
  -- Given n queries to oracle, what is worst case probability of compromise?
  (soundness_bound : ℕ -> ℝ≥0)
  -- Given an adversarial query generator and way of generating a proof from the queries, the extractor outputs the witness the generator knows
  (knowledge_extractor : (query_bounded_adversary ro_domain D Proof) -> Wit)
  (soundness :
    ∀ (query_count : ℕ), ∀ (stmt : Stmt),
      -- adversary query generator takes a input and outputs a next query
      ∀ (𝓐 : query_bounded_adversary ro_domain D Proof),
        -- If the adversary has probability greater than the soundness bound of convincing the verifier ...
        (let adv_verifies : pmf bool := do
          h <- pmf.uniform_of_fintype (ro_domain -> D)
          return (verifier h stmt (query_bounded_adversary.to_fun 𝓐 query_count h stmt))
        adv_verifies tt ≥ soundness_bound query_count)
          -- ... then the extractor obtains a correct witness.
          -> relation stmt (knowledge_extractor 𝓐))



-- def noninteractive_random_oracle_proof_scheme.prod (n_ro_codomain : ℕ)
--   (𝓐 𝓑 : noninteractive_random_oracle_proof_scheme n_ro_codomain) :
--   noninteractive_random_oracle_proof_scheme n_ro_codomain :=
-- { Stmt := 𝓐.Stmt × 𝓑.Stmt,
--   Wit := 𝓐.Wit × 𝓑.Wit,
--   relation := λ ⟨a_stmt, b_stmt⟩ ⟨a_wit, b_wit⟩, 𝓐.relation a_stmt a_wit ∧ 𝓑.relation b_stmt b_wit,
--   ro_domain := 𝓐.ro_domain ⊕ 𝓑.ro_domain,
--   n_ro_codomain := n_ro_codomain,
--   nonempty_ro := sorry,
--   fintype_ro := sorry,
--   Proof := 𝓐.Proof × 𝓑.Proof,
--   prover := λ ro ⟨a_stmt, b_stmt⟩ ⟨a_wit, b_wit⟩, 
--     ⟨𝓐.prover (ro ∘ sum.inl) a_stmt a_wit,  
--      𝓑.prover (ro ∘ sum.inr) b_stmt b_wit⟩,
--   verifier := λ ro ⟨a_stmt, b_stmt⟩ ⟨a_proof, b_proof⟩, 
--     𝓐.verifier (ro ∘ sum.inl) a_stmt a_proof && 𝓑.verifier (ro ∘ sum.inr) b_stmt b_proof,
--   completeness := 
--   begin
--     intros ro stmt wit rels_satisfied, 
--     cases wit with 𝓐_wit 𝓑_wit, 
--     cases stmt with 𝓐_stmt 𝓑_stmt, 
--     cases rels_satisfied with 𝓐_relation 𝓑_relation, 
--     unfold_coes, unfold_aux, unfold_aux, simp at *, fsplit,
--     exact 𝓐.completeness (ro ∘ sum.inl) 𝓐_stmt 𝓐_wit 𝓐_relation,
--     exact 𝓑.completeness (ro ∘ sum.inr) 𝓑_stmt 𝓑_wit 𝓑_relation,
--   end,
--   soundness_bound := λ n, 𝓐.soundness_bound n * 𝓑.soundness_bound n,
--   knowledge_extractor := sorry, -- Construct two query bounded adversarys by looking
--   soundness := 
--   begin
--     tidy,
--     sorry,
--   end, }

-- /-- Return a `noninteractive_random_oracle_proof_scheme` in the form of a general IOP: 
-- A scheme where the prover sends a sequence of merkle trees commiting to data using interaction to generate randomness. 
-- We then might prove soundness via the fiat-shamir transform -/
-- def interactive_oracle_proof_scheme :
--   noninteractive_random_oracle_proof_scheme := sorry
