/-
Copyright (c) 2023 RISC Zero. All rights reserved.
-/

import Mathlib.Data.Option.Basic
import Init.Data.Nat.Log2
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

## Merkle Tree Collision Extraction

The main def here is MerkleTreeCollisionExtractor, which,
given two merkle branches to the same index proving different values, 
returns a collision of the hash function.

There is also code for a proof of this fact. This proof is in the process of being ported from Lean 3, but a few more tactics will be needed first.

-/

-- variables  {F : Type} {A : Type} [decidable_eq A] [inhabited F] [inhabited A]

/-- A collection of functions representing a hash function which can act on a field type and also on pairs of its own hashes, 
as well as return field elements. -/
structure hashing_scheme (F A : Type) :=
  (of_hash_pair : A -> A -> A)
  (of_field_element : F -> A)
  -- (hash_to_field : A -> F) -- might readd later

variable (h : hashing_scheme F A)

def LayersFromRowSize (row_size : Nat) := Nat.log2 row_size

def TopLayerFromQueries (queries : Nat) := Nat.log2 (queries) + 1

-- Pairs elements off to make a row in the Merkle tree one layer closer to the root
def CollapseLayer : List A -> List A
| [] => []
| (a :: []) => a :: []
| (a :: b :: tail') => (h.of_hash_pair a b) :: (CollapseLayer tail')

/-- Takes a height and a List of hashes of length 2^height and returns the hash tree -/
def hash_tree (height : Nat) (hashes: List A) : List A :=
match height with
| 0 => hashes ++ hashes
| (n+1) => hash_tree n (CollapseLayer h hashes) ++ hashes


/-- A structure representing a merkle tree of elements of leaf type F and hash type A 
leaves is expected to be a List of power-of-2-length, and hashes a List of twice the length
The indexing of hashes is as follows: 
the first element is a default, 
the index one element is the root, 
the index 2-3 elements are the next row of the tree
and so forth, up to the n to (2*n-1)th elements, which are the indices of the hashes of the leaves-/
structure merkle_tree (F A : Type) :=
  (hashes : List A)
  (leaves : List F)

def merkle_tree.root [Inhabited A] (tree : merkle_tree F A) : A := tree.hashes.get! 1

/-- Contains the List of hashes and the leaf value field element. 
The head element of the branch hashes is closest to the leaves -/
structure MerkleBranch (F A : Type) :=
  (branch_hashes : List A)
  (leaf : F)

def merkle_tree.of_leaf_List (leaves : List F) : merkle_tree F A :=
{ hashes := hash_tree h (Nat.log2 leaves.length) (leaves.map h.of_field_element),
  leaves := leaves } 

def merkle_tree.layers (tree : merkle_tree F A) : Nat := (Nat.log2 tree.leaves.length)

/-- Given an index as described in the module docstring and a level, returns the descendant of that index in that level.
e.g. get_ancestor_at_level 1 7 should return 3. -/
def get_ancestor_at_level (level : Nat) (val : Nat): Nat :=
val / 2 ^ (Nat.log2 val - level)


def get_sibling (idx : Nat) : Nat :=
if idx % 2 = 1 then idx - 1 else idx + 1


def merkle_tree.get_branch [Inhabited F] [Inhabited A] (tree : merkle_tree F A) (idx : Nat) : MerkleBranch F A := 
{ branch_hashes := 
    (List.range tree.layers).map 
      (λ i => tree.hashes.get! (get_sibling (get_ancestor_at_level (tree.layers - i) idx))),
  leaf := tree.leaves.get! idx }

/-- Given `top_layer` a row of a Merkle tree at layer top, an index into the tree, 
a value at that index, and a Merkle branch putatively proving that value to that top row, 
this checks if the verification of the branch will return correct
-/
def merkle_tree.verify_aux [Inhabited A] [DecidableEq A] (hash : A -> A -> A) 
  (top_layer_idx : Nat) (top_layer : List A) (tree_idx: Nat) (value: A) (branch: List A):
  Bool := 
match branch with
| [] =>
    Nat.ble (2 ^ top_layer_idx) tree_idx
    && 
    Nat.blt tree_idx (2 ^ (Nat.succ top_layer_idx))
    && 
    List.get! top_layer (tree_idx - 2 ^ top_layer_idx) == some value
| (h :: tail) =>
    if tree_idx % 2 = 1
      then merkle_tree.verify_aux hash top_layer_idx top_layer (tree_idx / 2) (hash h value) tail
      else merkle_tree.verify_aux hash top_layer_idx top_layer (tree_idx / 2) (hash value h) tail


--  * Given `top` a row of a Merkle tree at layer top_layer, a row_size, an index into the row of leaves, a leaf value at that index, and a Merkle branch putatively proving that value to that top row, this checks if the verification of the branch will return correct *)
def MerkleTreeVerifyToTop [Inhabited A] [DecidableEq A] (top_layer : Nat) (top : List A) (row_size : Nat) (idx : Nat) (value : A) 
  (branch : List A) : Bool := 
merkle_tree.verify_aux h.of_hash_pair top_layer top (idx + row_size) value branch

--  Given a root of a Merkle tree, a row_size, an index into the row of leaves, a leaf value at that index, and a Merkle branch putatively proving that value to that top row, this checks if the verification of the branch will return correct *)
def MerkleTreeVerifyToRoot [Inhabited A] [DecidableEq A] (root : A) (row_size : Nat) (idx : Nat) (value : A) 
  (branch : List A) : Bool :=
MerkleTreeVerifyToTop h 0 (root :: []) row_size idx value branch

def merkle_branch.verify [Inhabited A] [DecidableEq A] (branch : MerkleBranch F A) (root_hash : A) (index : Nat) : Bool := 
MerkleTreeVerifyToRoot h root_hash (2^branch.branch_hashes.length) index 
  (h.of_field_element branch.leaf) (branch.branch_hashes)




structure MerkleTreeVerifier : Type :=
  (top_layer_idx : Nat)
  (top_layer : List A)




--     (* Given two merkle branches to two values at the same index, returns the first pair of pairs of hashes that result in the same value. *)
def MerkleTreeCollisionExtractor [Inhabited A] [DecidableEq A] (hash : A -> A -> A) 
  (idx : Nat) (value1 value2 : A) (branch1 branch2 : List A):
  (Option ((A × A) × (A × A))) :=
match branch1, branch2 with
| [], _ => none
| _, [] => none
| (h1 :: tail1), (h2 :: tail2) =>
  if idx % 2 = 1
    then if (hash h1 value1) == (hash h2 value2)
      then some ((h1, value1), (h2, value2))
      else MerkleTreeCollisionExtractor hash (idx / 2) (hash h1 value1) (hash h2 value2) tail1 tail2
    else if (hash value1 h1) == (hash value2 h2)
      then some ((value1, h1), (value2, h2))
      else MerkleTreeCollisionExtractor hash (idx / 2) (hash value1 h1) (hash value2 h2) tail1 tail2


--     (* A helper lemma telling us the length of a Merkle Branch *)
def BranchLengthOfMerkleTreeVerifyAux [Inhabited A] [DecidableEq A] 
  (hash: A -> A -> A)
  {top_layer_idx : Nat} {top_layer : List A} {tree_idx : Nat}
  {value : A} {branch : List A}
  (h_verifies : merkle_tree.verify_aux hash top_layer_idx top_layer tree_idx value branch) :
    List.length branch = (Nat.log2 tree_idx) - top_layer_idx := by
  revert h_verifies
  revert tree_idx
  revert value
  induction branch
  { intros value tree_idx h_verifies
    unfold merkle_tree.verify_aux at h_verifies
    sorry
    -- cases h_verifies with ⟨hva, hvb, hve⟩
    -- clear hve value top_layer
    -- simp
    -- symmetry
    -- rw tsub_eq_zero_iff_le
    -- have : (Nat.log2 tree_idx = top_layer_idx)
    -- {
    --   sorry,
    -- },
    -- rw this,
    -- apply Nat,log2_unique,
    -- apply Nat,le_0_l,
    -- split,
    -- auto,
    -- auto,
    -- rewrite H,
    -- apply Nat,le_refl,
  }
  {
    intros value tree_idx h_verifies
    simp
    sorry
    -- (* simpl in h_verifies, *)
    -- unfold MerkleTreeVerifyAux in h_verifies,
    -- fold MerkleTreeVerifyAux in h_verifies,

    -- destruct (tree_idx mod 2 =? 1 ),
    -- {
    --   apply (IHbranch (hash a value) (tree_idx / 2)) in h_verifies,
    --   rewrite h_verifies,
    --   (* Not quite true for tree_idx <= 1, add edge case*)
    --   admit,
    -- }
    -- {
    --   apply (IHbranch (hash value a) (tree_idx / 2)) in h_verifies,
    --   rewrite h_verifies,
    --   admit,
    -- }

  }



-- --     (* Proves the output of `MerkleTreeCollisionExtractor` returns two distinct values, when given two branches that both verify against that same top. *)
def MerkleTreeCollisionExtractor__CollidesAux
    [Inhabited A] [DecidableEq A]
    (hash: A -> A -> A)
    (top_layer : Nat) (top : List A) (row_size : Nat) -- (* A Merkle Tree *)
    (idx : Nat)       --  (* A specific leaf location *)
    (value1 value2 : A) -- (* Two (ostensibly different) leaf values *)
    (branch1 branch2 : List A) -- (* Two branches to those values *)
    (hbranch1 : merkle_tree.verify_aux hash top_layer top idx value1 branch1)
    (hbranch2 : merkle_tree.verify_aux hash top_layer top idx value2 branch2)
    (hneq : value1 ≠ value2) :
    (MerkleTreeCollisionExtractor hash idx value1 value2 branch1 branch2).casesOn' false (λ p => p.fst.fst = p.snd.fst ∧ p.fst.snd = p.snd.snd) :=
by
  revert value1 value2 idx branch2
  sorry
  -- induction branch1
  -- { --(* Case where one branch is empty *)
  --   intros value1 value2 idx branch2 hv1 hv2 neq
  --   pose proof BranchLengthOfMerkleTreeVerifyAux hv1 as H1
  --   pose proof BranchLengthOfMerkleTreeVerifyAux hv2 as H2
  --   rewrite <-H1 in H2.

  --   simpl.
  --   apply neq.

  --   simpl in H1.

  --   simpl in H2.
  --   rewrite length_zero_iff_nil in H2.
  --   rewrite H2 in hv2.
  --   unfold MerkleTreeVerifyAux in hv1.
  --   unfold MerkleTreeVerifyAux in hv2.
  --   destruct hv1 as [hv1a [hv1b hv1e]].
  --   destruct hv2 as [hv2a [hv2b hv2e]].
  --   rewrite hv1e in hv2e.
  --   inversion hv2e.
  --   rewrite (refl_eq value2).
  --   simpl.
  --   constructor.
  -- }
  -- intros value1 value2 idx branch2 hv1 hv2 neq.
  -- destruct branch2.
  -- {
  --   exfalso.
  --   pose proof BranchLengthOfMerkleTreeVerifyAux hv1 as H1.
  --   pose proof BranchLengthOfMerkleTreeVerifyAux hv2 as H2.
  --   rewrite <-H1 in H2.
  --   simpl in H2.
  --   inversion H2.
  -- }
  -- unfold MerkleTreeCollisionExtractor.
  -- fold MerkleTreeCollisionExtractor.
  -- unfold MerkleTreeVerifyAux in hv1.
  -- unfold MerkleTreeVerifyAux in hv2.
  -- fold MerkleTreeVerifyAux in hv1.
  -- fold MerkleTreeVerifyAux in hv2.

  -- destruct (idx mod 2 =? 1) eqn:?.
  -- { pose proof IHbranch1 (hash a value1) (hash a0 value2) (idx / 2) branch2 hv1 hv2.

  --   destruct (eq (hash a value1) (hash a0 value2)) eqn:?.
  --   {
  --     intro H2.
  --     apply neq.
  --     elim (andb_prop_elim _ _ H2).
  --     intros left_ right_.
  --     exact right_.
  --   }
  --   {
  --     apply H.
  --     simpl. (* What is the canonical way of solving this? *)
  --     intro f.
  --     exact f.
  --   }
  -- }
  -- { pose proof IHbranch1 (hash value1 a) (hash value2 a0) (idx / 2) branch2 hv1 hv2.
  --   destruct (eq (hash value1 a) (hash value2 a0)) eqn:?.
  --   {
  --     intro H2.
  --     apply neq.
  --     elim (andb_prop_elim _ _ H2).
  --     intros left_ right_.
  --     exact left_.
  --   }
  --   {
  --     apply H.
  --     simpl.
  --     intro f.
  --     exact f.
  --   }
  --   (* Similar to previous block *)
  -- },



