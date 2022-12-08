/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
-- import Zkvm.ArithVM.Circuit
-- import Zkvm.Constants
-- import Zkvm.Verify.Classes
-- import Zkvm.Verify.Merkle

namespace R0sy.Algebra.Ntt

/-- Reverses the bits of n, treating n as a b-bit binary number -/
partial def reverse_bits (n b : Nat) : Nat :=
  if b == 0
    then 0
    else 2 ^ (b-1) * (n % 2) + (reverse_bits (n / 2) (b - 1)) 

instance [Add T] : Add (Array T) where add x y := (Array.zip x y).map (λ (a,b) => a + b)

instance [Sub T] : Sub (Array T) where sub x y := (Array.zip x y).map (λ (a,b) => a - b)

partial def fwd_butterfly [Field ExtElem] [RootsOfUnity ExtElem] (n expand_bits : Nat) (arr : Array ExtElem) : Array ExtElem :=
  if n == 0
    then arr
    else if n == expand_bits 
      then arr
      else 
        let a : Array ExtElem := fwd_butterfly (n-1) expand_bits (arr.toSubarray 0 (2 ^ (n-1))).toArray;
        let b : Array ExtElem := 
          (Array.zip 
            (fwd_butterfly (n-1) expand_bits (arr.toSubarray (2 ^ (n-1)) (2 ^ n)).toArray) 
            (List.range n).toArray).map (λ (a, k) => a * (RootsOfUnity.ROU_FWD[n]! : ExtElem) ^ k);
        (a + b) ++ (a - b)

end R0sy.Algebra.Ntt
