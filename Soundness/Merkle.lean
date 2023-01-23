/-
Copyright (c) 2023 RISC Zero. All rights reserved.
-/

import Zkvm.Verify.Merkle

/-!
# Merkle Trees in Lean

Implements merkle trees

## Indexing

01            -- Level 0
|   \
02    03      -- Level 1
| \   | \ 
04 05 06 07   -- Level 2



-/

variables  {F : Type} {A : Type} [decidable_eq A] [inhabited F] [inhabited A]

/-- A collection of functions representing a hash function which can act on a field type and also on pairs of its own hashes, 
as well as return field elements. -/
structure hashing_scheme (F A : Type) :=
  (of_hash_pair : A -> A -> A)
  (of_field_element : F -> A)
  -- (hash_to_field : A -> F) -- might readd later

variable (h : hashing_scheme F A)

def LayersFromRowSize (row_size : nat) := nat.log 2 row_size.

def TopLayerFromQueries (queries : nat) := nat.log 2 (queries) + 1.

-- Pairs elements off to make a row in the Merkle tree one layer closer to the root
def CollapseLayer : list A -> list A
| [] := []
| (a :: []) := a :: []
| (a :: b :: tail') := (h.of_hash_pair a b) :: (CollapseLayer tail')

/-- Takes a height and a list of hashes of length 2^height and returns the hash tree -/
def hash_tree : ℕ -> list A -> list A 
| 0 xs := xs ++ xs
| (n+1) (xs) := hash_tree n (CollapseLayer h xs) ++ xs


/-- A structure representing a merkle tree of elements of leaf type F and hash type A 
leaves is expected to be a list of power-of-2-length, and hashes a list of twice the length
The indexing of hashes is as follows: 
the first element is a default, 
the index one element is the root, 
the index 2-3 elements are the next row of the tree
and so forth, up to the n to (2*n-1)th elements, which are the indices of the hashes of the leaves-/
structure merkle_tree (F A : Type) :=
  (hashes : list A)
  (leaves : list F)

def merkle_tree.root (tree : merkle_tree F A) : A := tree.hashes.inth 1

/-- Contains the list of hashes and the leaf value field element. 
The head element of the branch hashes is closest to the leaves -/
structure MerkleBranch (F A : Type) :=
  (branch_hashes : list A)
  (leaf : F)

def merkle_tree.of_leaf_list (leaves : list F) : merkle_tree F A :=
{ hashes := hash_tree h (nat.log 2 leaves.length) (leaves.map h.of_field_element),
  leaves := leaves } 

def merkle_tree.layers (tree : merkle_tree F A) : ℕ := (nat.log 2 tree.leaves.length)

/-- Given an index as described in the module docstring and a level, returns the descendant of that index in that level.
e.g. get_ancestor_at_level 1 7 should return 3. -/
def get_ancestor_at_level (level : ℕ) (val : ℕ): ℕ :=
val / 2 ^ (nat.log 2 val - level)


def get_sibling (idx : ℕ) : ℕ :=
if idx % 2 = 1 then idx - 1 else idx + 1


def merkle_tree.get_branch (tree : merkle_tree F A) (idx : ℕ) : MerkleBranch F A := 
{ branch_hashes := 
    (list.range tree.layers).map 
      (λ i, tree.hashes.inth (get_sibling (get_ancestor_at_level (tree.layers - i) idx))),
  leaf := tree.leaves.inth idx }

/-- Given `top_layer` a row of a Merkle tree at layer top, an index into the tree, 
a value at that index, and a Merkle branch putatively proving that value to that top row, 
this checks if the verification of the branch will return correct
-/
def merkle_tree.verify_aux (hash : A -> A -> A) :
ℕ -> list A -> ℕ -> A -> list A -> bool
| top_layer_idx  top_layer tree_idx value [] :=
    2 ^ top_layer_idx ≤ tree_idx
    ∧ tree_idx < 2 ^ (nat.succ top_layer_idx)
    ∧ list.nth top_layer (tree_idx - 2 ^ top_layer_idx) = some value
| top_layer_idx  top_layer tree_idx value  (h :: tail) :=
    if tree_idx % 2 = 1
      then merkle_tree.verify_aux top_layer_idx top_layer (tree_idx / 2) (hash h value) tail
      else merkle_tree.verify_aux top_layer_idx top_layer (tree_idx / 2) (hash value h) tail


--  * Given `top` a row of a Merkle tree at layer top_layer, a row_size, an index into the row of leaves, a leaf value at that index, and a Merkle branch putatively proving that value to that top row, this checks if the verification of the branch will return correct *)
def MerkleTreeVerifyToTop (top_layer : nat) (top : list A) (row_size : nat) (idx : nat) (value : A) 
  (branch : list A) : bool := 
merkle_tree.verify_aux h.of_hash_pair top_layer top (idx + row_size) value branch

--  Given a root of a Merkle tree, a row_size, an index into the row of leaves, a leaf value at that index, and a Merkle branch putatively proving that value to that top row, this checks if the verification of the branch will return correct *)
def MerkleTreeVerifyToRoot (root : A) (row_size : nat) (idx : nat) (value : A) 
  (branch : list A) : bool :=
MerkleTreeVerifyToTop h 0 (root :: []) row_size idx value branch

def merkle_branch.verify (branch : MerkleBranch F A) (root_hash : A) (index : ℕ) : bool := 
MerkleTreeVerifyToRoot h root_hash (2^branch.branch_hashes.length) index 
  (h.of_field_element branch.leaf) (branch.branch_hashes)




structure MerkleTreeVerifier : Type :=
  (top_layer_idx : nat)
  (top_layer : list A)



/-!
# Merkle Tree Collision Extraction

A file for lemma about collision extraction in Merkle trees. 

Main def is MerkleTreeCollisionExtractor, which, 
given two merkle branches to the same index proving different values, 
returns a collision of the hash function.

-/


--     (* Given two merkle branches to two values at the same index, returns the first pair of pairs of hashes that result in the same value. *)
-- def MerkleTreeCollisionExtractor :
--   ℕ -> A -> A -> list A -> list A -> (option ((A × A) × (A × A)))
-- | idx value1 value2 [] _ := none
-- | idx value1 value2 _ [] := none
-- | idx value1 value2 (h1 :: tail1) (h2 :: tail2) :=
--   if idx % 2 = 1
--     then if eq (hash h1 value1) (hash h2 value2)
--       then some ((h1, value1), (h2, value2))
--       else MerkleTreeCollisionExtractor (idx / 2) (hash h1 value1) (hash h2 value2) tail1 tail2
--     else if eq (hash value1 h1) (hash value2 h2)
--       then some ((value1, h1), (value2, h2))
--       else MerkleTreeCollisionExtractor (idx / 2) (hash value1 h1) (hash value2 h2) tail1 tail2


--     (* A helper lemma telling us the length of a Merkle Branch *)
-- lemma BranchLengthOfMerkleTreeVerifyAux {top_layer_idx : nat} {top_layer : list A} {tree_idx : nat}
--   {value : A} {branch : list A}
--   (h_verifies : MerkleTreeVerifyAux hash top_layer_idx top_layer tree_idx value branch) :
--   list.length branch = (nat.log 2 tree_idx) - top_layer_idx :=
-- begin
--   revert h_verifies,
--   revert tree_idx,
--   revert value,
--   induction branch,
--   { intros value tree_idx h_verifies,
--     unfold MerkleTreeVerifyAux at h_verifies,
--     rcases h_verifies with ⟨hva, hvb, hve⟩,
--     clear hve value top_layer,
--     simp,
--     symmetry,
--     rw tsub_eq_zero_iff_le,
--     have : (nat.log 2 tree_idx = top_layer_idx),
--     {
--       sorry,
--     },
--     rw this,
--     -- apply Nat,log2_unique,
--     -- apply Nat,le_0_l,
--     -- split,
--     -- auto,
--     -- auto,
--     -- rewrite H,
--     -- apply Nat,le_refl,
--   },
--   {
--     intros value tree_idx h_verifies,
--     simp,
--     sorry,
--     -- (* simpl in h_verifies, *)
--     -- unfold MerkleTreeVerifyAux in h_verifies,
--     -- fold MerkleTreeVerifyAux in h_verifies,

--     -- destruct (tree_idx mod 2 =? 1 ),
--     -- {
--     --   apply (IHbranch (hash a value) (tree_idx / 2)) in h_verifies,
--     --   rewrite h_verifies,
--     --   (* Not quite true for tree_idx <= 1, add edge case*)
--     --   admit,
--     -- }
--     -- {
--     --   apply (IHbranch (hash value a) (tree_idx / 2)) in h_verifies,
--     --   rewrite h_verifies,
--     --   admit,
--     -- }

--   },
-- end

-- --     (* Proves the output of `MerkleTreeCollisionExtractor` returns two distinct values, when given two branches that both verify against that same top. *)
-- lemma MerkleTreeCollisionExtractor__CollidesAux
--     (top_layer : nat) (top : list A) (row_size : nat) -- (* A Merkle Tree *)
--     (idx : nat)       --  (* A specific leaf location *)
--     (value1 value2 : A) -- (* Two (ostensibly different) leaf values *)
--     (branch1 branch2 : list A) -- (* Two branches to those values *)
--     (hbranch1 : MerkleTreeVerifyAux hash top_layer top idx value1 branch1)
--     (hbranch2 : MerkleTreeVerifyAux hash top_layer top idx value2 branch2)
--     (hneq : value1 ≠ value2) :
--     (MerkleTreeCollisionExtractor hash idx value1 value2 branch1 branch2).cases_on' false (λ p, p.fst.fst = p.snd.fst ∧ p.fst.snd = p.snd.snd) :=
-- begin
--   sorry,
--         -- revert value1 value2 idx branch2,
--         -- induction branch1.
--         -- { (* Case where one branch is empty *)
--         --   intros value1 value2 idx branch2 hv1 hv2 neq.
--         --   pose proof BranchLengthOfMerkleTreeVerifyAux hv1 as H1.
--         --   pose proof BranchLengthOfMerkleTreeVerifyAux hv2 as H2.
--         --   rewrite <-H1 in H2.

--         --   simpl.
--         --   apply neq.

--         --   simpl in H1.

--         --   simpl in H2.
--         --   rewrite length_zero_iff_nil in H2.
--         --   rewrite H2 in hv2.
--         --   unfold MerkleTreeVerifyAux in hv1.
--         --   unfold MerkleTreeVerifyAux in hv2.
--         --   destruct hv1 as [hv1a [hv1b hv1e]].
--         --   destruct hv2 as [hv2a [hv2b hv2e]].
--         --   rewrite hv1e in hv2e.
--         --   inversion hv2e.
--         --   rewrite (refl_eq value2).
--         --   simpl.
--         --   constructor.
--         -- }
--         -- intros value1 value2 idx branch2 hv1 hv2 neq.
--         -- destruct branch2.
--         -- {
--         --   exfalso.
--         --   pose proof BranchLengthOfMerkleTreeVerifyAux hv1 as H1.
--         --   pose proof BranchLengthOfMerkleTreeVerifyAux hv2 as H2.
--         --   rewrite <-H1 in H2.
--         --   simpl in H2.
--         --   inversion H2.
--         -- }
--         -- unfold MerkleTreeCollisionExtractor.
--         -- fold MerkleTreeCollisionExtractor.
--         -- unfold MerkleTreeVerifyAux in hv1.
--         -- unfold MerkleTreeVerifyAux in hv2.
--         -- fold MerkleTreeVerifyAux in hv1.
--         -- fold MerkleTreeVerifyAux in hv2.

--         -- destruct (idx mod 2 =? 1) eqn:?.
--         -- { pose proof IHbranch1 (hash a value1) (hash a0 value2) (idx / 2) branch2 hv1 hv2.

--         --   destruct (eq (hash a value1) (hash a0 value2)) eqn:?.
--         --   {
--         --     intro H2.
--         --     apply neq.
--         --     elim (andb_prop_elim _ _ H2).
--         --     intros left_ right_.
--         --     exact right_.
--         --   }
--         --   {
--         --     apply H.
--         --     simpl. (* What is the canonical way of solving this? *)
--         --     intro f.
--         --     exact f.
--         --   }
--         -- }
--         -- { pose proof IHbranch1 (hash value1 a) (hash value2 a0) (idx / 2) branch2 hv1 hv2.
--         --   destruct (eq (hash value1 a) (hash value2 a0)) eqn:?.
--         --   {
--         --     intro H2.
--         --     apply neq.
--         --     elim (andb_prop_elim _ _ H2).
--         --     intros left_ right_.
--         --     exact left_.
--         --   }
--         --   {
--         --     apply H.
--         --     simpl.
--         --     intro f.
--         --     exact f.
--         --   }
--         --   (* Similar to previous block *)
--         -- },
-- end



