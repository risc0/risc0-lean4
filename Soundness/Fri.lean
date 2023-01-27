
import Mathlib.Algebra.GroupPower.Ring
import Mathlib.Algebra.Field.Defs
-- import Mathlib.Algebra.BigOperators.Basic

import Std.Data.Nat.Lemmas
-- import data.polynomial.basic
-- import data.polynomial.degree
-- import group_theory.group_action.pi
-- import group_theory.group_action.defs
-- import data.fintype.basic
-- import data.finset.lattice
-- import data.matrix.basic
-- import data.set.function
-- import probability.probability_mass_function.constructions
-- import .merkle
-- import ...proof_system
-- import data.mv_polynomial.basic
-- import linear_algebra.lagrange
-- import logic.equiv.fin

open_locale big_operators

-- variables {F : Type} [field F] [fintype F] [inhabited F] [decidable_eq F]
-- variables {Hash : Type} [inhabited Hash] [decidable_eq Hash]

-- open polynomial

/-- A structure which describes the parameters of a FRI instantiation, including things like the 
folding factor, the number of rounds and queries per round, and the root of unity. -/
structure fri_size_parameters (F : Type) :=
  (log_2_folding_factor rounds degree queries : Nat)
  (log_2_folding_factor_nonzero : log_2_folding_factor ≠ 0)
  (rou : F)

def fri_size_parameters.folding_factor (params : fri_size_parameters F) := 
2 ^ (params.log_2_folding_factor)

def fri_size_parameters.folding_factor_nonzero (params : fri_size_parameters F) : params.folding_factor ≠ 0 := 
by
  rw [fri_size_parameters.folding_factor]
  apply pow_ne_zero
  simp


def fri_size_parameters.max_degree_of_round (params : fri_size_parameters F) (round : Nat) := 
params.degree / (params.folding_factor) ^ round

def fri_size_parameters.round_rou [Field F] (params : fri_size_parameters F) (round : Nat) := 
params.rou ^ (params.folding_factor ^ round)

instance has_zero_fin_pow (n : Nat) : Zero (Fin (2^n)) := 
by
  rw [←@Nat.succ_pred_eq_of_pos (2^n)]
  sorry
  simp


instance inhabited_fin_pow (n : Nat) : Inhabited (Fin (2^n)) := ⟨0⟩

instance fin_folding_factor_has_zero (params : fri_size_parameters F) : Zero (Fin params.folding_factor) := 
by
  unfold fri_size_parameters.folding_factor
  apply has_zero_fin_pow


def fri_size_parameters.max_degree (params : fri_size_parameters F) := params.folding_factor ^ params.rounds

-- TODO replace when mathlib ports polynomials
def Polynomial (F : Type) [Field F] := List F 

def Polynomial.nat_degree [Field F] (p : Polynomial F) : Nat := sorry

def Polynomial.coeff [Field F] (p : Polynomial F) (n : Nat) : F := sorry

instance (F : Type) [Field F] : Zero (Polynomial F) := sorry

-- Return the splits of a polynomial
-- noncomputability could be a problem later when trying to run this, consider switching to the code from
-- https://github.com/leanprover-community/mathlib/pull/15088
noncomputable def polynomial_split [Field F] (params : fri_size_parameters F) 
  (p : Polynomial F)
  (split : Fin params.folding_factor) : Polynomial F := sorry
-- { to_finsupp :=  (p.to_finsupp).comap_domain (λ n => (params.folding_factor * n + (split : Nat))) (by
--   -- prove this coefficient map is injective
--   apply set.inj_on_of_injective
--   intros a1 a2 h
--   -- simp [params.folding_factor_nonzero] at h
--   -- exact h,
-- ), }
--   -- ⟨p.to_finsupp.filter (λ n, (n % folding_factor = (split : Nat)))⟩
-- -- polynomial.of_finsupp ({ support := _,
-- --   to_fun := λ n, p.to_finsupp (n * folding_factor + split : Nat),
-- --   mem_support_to_fun := _ })

def nat_degree_lt_iff_coeff_eq_zero [Field F] {p : Polynomial F} (n : Nat) (n_pos : 0 < n) :
  p.nat_degree < n ↔ ∀ N : Nat, n ≤ N → p.coeff N = 0 :=
by
  by_cases h : p = 0
  { simp [h, n_pos]
    sorry }
  sorry -- rw [nat_degree_lt_iff_degree_lt h, degree_lt_iff_coeff_zero]


def polynomial_split_degree_lt [Field F] (params : fri_size_parameters F) (n : Nat) (p : Polynomial F)
  (split : Fin params.folding_factor) (h : p.nat_degree < n * params.folding_factor) : (polynomial_split params p split).nat_degree < n :=
by
  rw [nat_degree_lt_iff_coeff_eq_zero] at *
  intros N hN
  unfold polynomial_split
  sorry
  sorry
  sorry
  -- -- simp only [finsupp.comap_domain_apply, polynomial.coeff.equations._eqn_1]
  -- rw ←polynomial.coeff,
  -- simp only [polynomial.coeff],
  -- sorry,
  -- sorry,
  -- sorry,
  -- -- apply h,


-- A polynomial is a linear combination of its splits evaluated at X^folding_factor
def sum_polynomial_split (params : fri_size_parameters F)  (p : Polynomial F) (val : F) :
  ∑ split : Fin params.folding_factor,
    ((polynomial_split params p split).eval val^params.folding_factor) * val ^ (split : Nat)
  = p.eval val :=
by
  unfold polynomial_split,
  -- ext,
  -- simp,
  sorry,


/-- Given a polynomial, root_of_unity, returns the merkle_tree committing to the evaluations of the polynomial -/
def polynomial_merkle_tree [Field F] (h : hashing_scheme F Hash) (p : Polynomial F) (root_of_unity : F) (degree : Nat) : merkle_tree F Hash :=
merkle_tree.of_leaf_list h ((list.range degree).map (λ i => p.eval (root_of_unity^i)))

variable (hash_to_field : Hash -> F)
variable (hash_to_nat : Hash -> Nat)

/-- Returns a mixing parameter from a hash that will be used to mix the splits of the polynomial. 
TODO, should this depend on previous polynomials as well? -/
def get_mixing_parameter (params : fri_size_parameters F) (hash : Hash) : F :=
hash_to_field hash

/-- Given a polynomial and a mixing parameter, computes the mixture of the splits of the polynomial -/
noncomputable def mix (params : fri_size_parameters F) (p : Polynomial F) (α : F) :=
∑ (split : Fin params.folding_factor), (α)^(split : Nat) • (polynomial_split params p split)

-- Returns, for protocol parameters and a polynomial, the sequence of polynomials that the fri protocol will produce when proving the degree of the given polynomial
noncomputable def fri_polynomial (params : fri_size_parameters F) (h : hashing_scheme F Hash) (p : Polynomial F) (root_of_unity : F) (degree : Nat) : Nat -> Polynomial F
| 0 := p
| (n+1) := 
  mix (params) (fri_polynomial n) 
    (hash_to_field
      --@get_mixing_parameter F _ _ _ _ Hash _ _ 
        -- params 
        (polynomial_merkle_tree h (fri_polynomial n) (root_of_unity) (degree / params.folding_factor ^ n)).root)



-- In what follows:
-- lo is the lower-degree polynomial (the higher index into polynomial_merkle_roots i+1)
-- hi is the higher-degree polynomial (the lower index into polynomial_merkle_roots i)

def offset_from_round_query (params : fri_size_parameters F) (merkle_roots : Fin (params.rounds + 1) → Hash)
  (round : Nat) (query : Fin params.queries) : Nat := sorry

/--
Returns, for protocol parameters, a random choice of offset, and a round, indices for the hi_queries that the IOP will make.
In other words, it returns the exponent of the root of unity for that round corresponding to the merkle branch
These should satisfy that they equal the corresponding lo_query_index when raised to the power of the folding_factor
-/
def hi_query_index (params : fri_size_parameters F) (offset : Nat) 
  (round : Nat) (query : Fin params.queries) (split : Fin params.folding_factor) : Nat :=
(offset) + (params.max_degree_of_round round) * (split : Nat)
-- hash all the merkle roots together, then hash in the round and query and take the result mod by what the params say the number of elements is.

/-- Returns, for protocol parameters, a random choice of offset, and a round, indices for the lo_queries that the IOP will make. TODO perhaps this should take the merkle roots list as input -/ 
def lo_query_index (params : fri_size_parameters F) (offset : Nat) 
  (round : Nat) (query : Fin params.queries) : Nat :=
(hi_query_index params offset round query 0) ^ (params.folding_factor)

/-- Returns a matrix which transforms the evaluations of p at `offset + degree * i` to the evaluations of the splits at offset * folding_factor -/
def hi_queries_to_splits (params : fri_size_parameters F) (round : Nat) (offset : Nat) 
  (i split : Fin params.folding_factor) : F :=
(params.round_rou round) ^ (-i * split : ℤ) --divide 16?

-- Returns a vector which transforms the evaluations of  the split polynomials to the evaluation of the lo_polynomial
def splits_to_lo_query (params : fri_size_parameters F) (α : F) 
  (split : Fin params.folding_factor) : F :=
α ^ (split : ℤ)

-- The coefficient of a query for a lower-degree polynomial in the one-level-higher polynomial
def query_component (params : fri_size_parameters F) (round : Nat) (offset : Nat) (α : F) : Fin params.folding_factor -> F := 
matrix.mul_vec 
  (hi_queries_to_splits params round offset) 
  (splits_to_lo_query params α)



/--
A single consistency test for round i of the FRI protocol consists of:
  a single                evaluation of the lower-degree  polynomial (i.e. Polynomial from round i+1)
  folding_factor (say 2) evaluations of the higher-degree polynomial (i.e. Polynomial from round i)
the lo_polynomial_branches entry contains, for any round i, and any query number q, 
  the merkle branch containing and proving that evaluation
the hi_polynomial_branches entry contains, for any round i, any query number q, and an index into the folding factor?,
  the merkle branch containing and proving that evaluation

lo is the lower-degree polynomial (the higher round number i+1)
hi is the higher-degree polynomial (the lower round number i)
-/
structure fri_proof (F Hash : Type) [field F] [fintype F] [inhabited F] [decidable_eq F] (params : fri_size_parameters F) :=
  (polynomial_merkle_roots : Fin (params.rounds + 1) -> Hash)
  (lo_polynomial_branches : Fin (params.rounds) -> Fin params.queries -> MerkleBranch F Hash)
  (hi_polynomial_branches : Fin (params.rounds) -> Fin params.queries -> Fin (params.folding_factor) -> MerkleBranch F Hash)
-- TODO why is inhabited necessary, should it not inherit from [field F]?


/-- The function which outputs the fri_proof for a particular hash function and polynomial -/
noncomputable def fri_prover (params : fri_size_parameters F) {h : hashing_scheme F Hash} (p : Polynomial F) (ω : F) (degree : Nat) : fri_proof F Hash params := 
let trees := λ round, 
  (polynomial_merkle_tree 
    h
    (fri_polynomial hash_to_field params h p ω degree (round : Nat)) 
    (ω ^ (params.folding_factor ^ (round : Nat)))
    (degree / (params.folding_factor ^ (round : Nat)))) in
let merkle_roots := λ (round : Fin (params.rounds + 1)), 
    (merkle_tree.root (trees round)) in
{ polynomial_merkle_roots := merkle_roots,
  lo_polynomial_branches := λ round q,
    merkle_tree.get_branch (trees (round + 1)) (lo_query_index params (hash_to_nat (merkle_roots 0)) round q),
  hi_polynomial_branches := λ round q split,
    merkle_tree.get_branch (trees (round)) (hi_query_index params (hash_to_nat (merkle_roots 0)) round q split), } -- TODO fix merkle_roots 0, instead combine hashes



/-- Converts a multi-input-type hash to a hashing scheme. 
This is necessary because in the proof_system file, we work in the random oracle model and we 
assume that the hash function is modeled as a simple function with a specific input and output type
-/
def multi_input_type_hash_to_hashing_scheme (mith : Hash × Hash ⊕ F → Hash) : hashing_scheme F Hash :=
{ of_hash_pair := λ x y, mith (sum.inl ⟨x, y⟩),
  of_field_element := λ f, mith (sum.inr f) }

noncomputable def fri (params : fri_size_parameters F) (hash_bits : Nat) 
  (hash_to_field : Fin (2^hash_bits) -> F) (hash_to_nat : Fin (2^hash_bits) -> Nat) : -- TODO some properties of hash_to_field and hash_to_nat will be needed
  noninteractive_random_oracle_proof_scheme (2^hash_bits) :=
{ Stmt := Polynomial F,
  Wit := unit,
  relation := λ p _, p.degree ≤ params.max_degree,
  -- Input to ro is either two hashes, or a field element
  ro_domain := ((Fin (2^hash_bits)) × (Fin (2^hash_bits))) ⊕ F,
  nonempty_ro := sorry,
  fintype_ro := sorry,
  Proof := fri_proof F (Fin (2^hash_bits)) params,
  -- The prover is given by `fri_prover`. 
  -- We must convert the multi-input-type hash into a hashing scheme for this
  prover := λ mith p _, @fri_prover F _ _ _ _ (Fin (2^hash_bits)) _ _ 
    hash_to_field hash_to_nat params (multi_input_type_hash_to_hashing_scheme mith) p params.rou params.degree,

  verifier := λ mith p proof,
    -- all lo branches verify
    (∀ round : Fin (params.rounds),
      ∀ query : Fin (params.queries),
        merkle_branch.verify
          (multi_input_type_hash_to_hashing_scheme mith)
          (proof.lo_polynomial_branches round query)
          (proof.polynomial_merkle_roots (round + 1))
          (lo_query_index params (offset_from_round_query params proof.polynomial_merkle_roots round query) round query))
    ∧
    -- all hi branches verify
    (∀ round : Fin (params.rounds),
      ∀ query : Fin (params.queries),
        ∀ ff : Fin (params.folding_factor),
          merkle_branch.verify
            (multi_input_type_hash_to_hashing_scheme mith)
            (proof.hi_polynomial_branches round query ff)
            (proof.polynomial_merkle_roots (round + 1))
            (hi_query_index params (offset_from_round_query params proof.polynomial_merkle_roots round query) round query ff))
    ∧
    -- all checks of lo values against hi values pass
    (∀ round : Fin (params.rounds),
      ∀ query : Fin (params.queries),
        ∑ ff : Fin (params.folding_factor),
          (proof.hi_polynomial_branches round query ff).leaf 
          * 
          (query_component 
            params 
            round 
            (offset_from_round_query params proof.polynomial_merkle_roots round query) (hash_to_field (proof.polynomial_merkle_roots 0) ) ff) --todo add mixing param
          = (proof.lo_polynomial_branches round query).leaf),
  completeness := sorry,
  soundness_bound := sorry,
  knowledge_extractor := sorry,
  soundness := sorry }





-- An execution trace is a matrix of field elements
def execution_trace {F : Type} [field F] (cols rows : Nat) := Fin cols -> Fin rows -> F 

-- -- A rule-checking polynomial on an execution trace is a mv_polynomial on variables representing cells in the trace with respect to a certain row, over the field
-- def check {F : Type} [field F] (cols : Nat) := mv_polynomial (Fin cols × Nat) F

-- ω is a expansion_factor * cols root of unity
/-- Given a trace, and an expansion factor, blow up the columns -/
noncomputable def interpolate_trace  {F : Type} [field F] {cols rows : Nat} (expansion_factor : Nat) (ω : F)
  (trace : @execution_trace F _ cols rows) : @execution_trace F _ cols (expansion_factor * rows) := 
λ c k, 
  (lagrange.interpolate 
    (finset.univ)
    (λ i : Fin rows, ω ^ (expansion_factor * i : Nat))
    (trace c)).eval (ω ^ (k : Nat))


-- TODO check direction of rotation
def fin_rotaten {n : Nat} (a : Fin n) (b : Nat) : Fin n := (equiv.to_fun (fin_rotate n))^[b] a

-- Given a constraint checked, a root of unity and an interpolated_trace, compute the constraint polynomial
noncomputable def constraint_polynomial  {F : Type} [field F] {cols rows : Nat}     
  (expansion_factor : Nat) (ω : F) 
  (constraint : mv_polynomial (Fin cols × Nat) F)
  (interpolated_trace : @execution_trace F _ cols (expansion_factor * rows)) : 
  Polynomial F := 
(lagrange.interpolate 
  (finset.univ)
  (λ i : Fin (expansion_factor * rows), ω ^ (i : Nat))
  (λ i, mv_polynomial.eval 
          (λ ⟨col, n⟩, 
            interpolated_trace col (fin_rotaten i (n * expansion_factor)) : (Fin cols × Nat) -> F) constraint))


def zeroes_polynomial {F : Type} [field F] (cols rows : Nat)     
  (expansion_factor : Nat) (ω : F) : Polynomial F := sorry

-- A constraint is satisfied on the trace iff the constraint polynomial is a multiple of the zeros polynomial
def constraint_satisfied_iff_zeros_polynomial_dvd_constraint_polynomial 
  {F : Type} [field F] {cols rows : Nat} (expansion_factor : Nat) (ω : F)
  (constraint : mv_polynomial ((Fin cols) × (Nat)) F) 
  (trace : @execution_trace F _ cols rows) : 
  (∀ row : Fin rows, 
    mv_polynomial.eval 
      (λ x : Fin cols × Nat, trace (x.fst) (fin_rotaten row x.snd)) 
      constraint = 0 )
  ↔ 
  (zeroes_polynomial cols rows expansion_factor ω) 
  ∣ 
  (constraint_polynomial expansion_factor ω constraint (interpolate_trace expansion_factor ω trace)) := 
sorry




