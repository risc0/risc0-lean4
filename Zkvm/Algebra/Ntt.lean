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

import R0sy
import Zkvm.Algebra.Classes

namespace Zkvm.Algebra.Ntt

open R0sy.Lean.Nat
open Zkvm.Algebra.Classes

def bit_rev_32 (in_x: UInt32): UInt32
  := Id.run do
      let mut x := in_x
      x := ((x &&& 0xaaaaaaaa) >>> 1) ||| ((x &&& 0x55555555) <<< 1)
      x := ((x &&& 0xcccccccc) >>> 2) ||| ((x &&& 0x33333333) <<< 2)
      x := ((x &&& 0xf0f0f0f0) >>> 4) ||| ((x &&& 0x0f0f0f0f) <<< 4)
      x := ((x &&& 0xff00ff00) >>> 8) ||| ((x &&& 0x00ff00ff) <<< 8)
      (x >>> 16) ||| (x <<< 16)

def bit_reverse [Inhabited T] (io: Array T): Array T
  := Id.run do
      let mut out := io
      let n := Nat.log2_ceil io.size
      if 1 <<< n != io.size then panic! s!"n:{n} io.size:{io.size}"
      for i in [0:io.size] do
        let rev_idx := (bit_rev_32 i.toUInt32).toNat >>> (32 - n)
        if rev_idx >= out.size then panic! s!"n:{n} i:{i} rev_idx:{rev_idx} out.size:{out.size}"
        if i < rev_idx then do
          let x := out[i]!
          let y := out[rev_idx]!
          out := Array.set! out i y
          out := Array.set! out rev_idx x
      pure out

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
        let first_half : Array ExtElem := fwd_butterfly (n-1) expand_bits (arr.toSubarray 0 (2 ^ (n-1))).toArray
        let second_half : Array ExtElem := fwd_butterfly (n-1) expand_bits (arr.toSubarray (2 ^ (n-1)) (2 ^ n)).toArray
        let second_half' : Array ExtElem := 
          second_half * 
          ((RootsOfUnity.ROU_FWD[n]! : ExtElem) ^ ((List.range n).toArray) : Array ExtElem)
        (first_half + second_half') ++ (first_half - second_half')

partial def rev_butterfly [Field ExtElem] [RootsOfUnity ExtElem] (n : Nat) (arr : Array ExtElem) : Array ExtElem :=
  match n with
  | 0 => arr
  | n + 1 =>
      let half := 1 <<< n
      let step: ExtElem := RootsOfUnity.ROU_REV[n + 1]!
      let a : Array ExtElem := arr.toSubarray 0 half
      let b : Array ExtElem := arr.toSubarray half (2 * half)
      let first_half  : Array ExtElem := rev_butterfly n <| a + b
      let second_half : Array ExtElem := rev_butterfly n <| (a - b) * (step ^ ((List.range half).toArray) : Array ExtElem)
      first_half ++ second_half

def interpolate_ntt [Field ExtElem] [RootsOfUnity ExtElem] (io : Array ExtElem) : Array ExtElem
  := Id.run do
      let n := Nat.log2_ceil io.size
      (rev_butterfly n io) * (Field.inv (Ring.ofNat io.size : ExtElem))

end Zkvm.Algebra.Ntt
