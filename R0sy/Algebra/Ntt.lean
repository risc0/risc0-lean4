/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import R0sy.Lean.Nat

namespace R0sy.Algebra.Ntt

open R0sy.Lean.Nat

/-- Reverses the bits of n, treating n as a b-bit binary number -/
partial def reverse_bits (n b : Nat) : Nat :=
  if b == 0
    then 0
    else 2 ^ (b-1) * (n % 2) + (reverse_bits (n / 2) (b - 1)) 

def bit_reverse [Inhabited T] (io : Array T) : Array T :=
  ((List.range io.size).map (λ i => io[reverse_bits i (Nat.log2_ceil io.size)]!)).toArray

instance [Add T] : Add (Array T) where add x y := Array.zipWith x y (λ a b => a + b)

instance [Sub T] : Sub (Array T) where sub x y := Array.zipWith x y (λ a b => a - b)

instance [Mul T] : Mul (Array T) where mul x y := Array.zipWith x y (λ a b => a * b)

instance [Mul T] : HMul (Array T) T (Array T) where hMul x y := x.map (λ a => a * y)

instance [HPow S T S] : HPow S (Array T) (Array S) where hPow x n := n.map (λ k => x ^ k)

partial def fwd_butterfly [Field ExtElem] [RootsOfUnity ExtElem] (n expand_bits : Nat) (arr : Array ExtElem) : Array ExtElem :=
  if n == 0
    then arr
    else if n == expand_bits 
      then arr
      else 
        let first_half : Array ExtElem := fwd_butterfly (n-1) expand_bits (arr.toSubarray 0 (2 ^ (n-1))).toArray;
        let second_half : Array ExtElem := fwd_butterfly (n-1) expand_bits (arr.toSubarray (2 ^ (n-1)) (2 ^ n)).toArray;
        let second_half' : Array ExtElem := 
          second_half * 
          ((RootsOfUnity.ROU_FWD[n]! : ExtElem) ^ ((List.range n).toArray) : Array ExtElem);
        (first_half + second_half') ++ (first_half - second_half')

partial def rev_butterfly [Field ExtElem] [RootsOfUnity ExtElem] (n : Nat) (arr : Array ExtElem) : Array ExtElem :=
  if n == 0
    then arr
    else 
      let a : Array ExtElem := (arr.toSubarray 0 (2 ^ (n-1)));
      let b : Array ExtElem := (arr.toSubarray (2 ^ (n-1)) (2 ^ n));
      let first_half : Array ExtElem := a + b;
      let second_half : Array ExtElem := (a - b) * ((RootsOfUnity.ROU_REV[n]! : ExtElem) ^ ((List.range n).toArray) : Array ExtElem);
      first_half ++ second_half

partial def interpolate_ntt [Field ExtElem] [RootsOfUnity ExtElem] (io : Array ExtElem) : Array ExtElem :=
  let n := Nat.log2_ceil io.size;
  (rev_butterfly n io) * (Field.inv (Field.fromUInt64 io.size.toUInt64 : ExtElem))

end R0sy.Algebra.Ntt
