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

namespace Zkvm.Algebra.Classes

open R0sy.Hash
open R0sy.Serial

class Ring (R: Type)
  extends
    ToString R,
    Inhabited R,
    BEq R,
    Add R,
    Neg R,
    Sub R,
    Mul R,
    HPow R Nat R
  where
    ofNat: Nat -> R
    zero: R := ofNat 0
    one: R := ofNat 1

class Field (F: Type)
  extends
    Ring F,
    Div F,
    SerialUInt32 F
  where
    inv: F -> F
    random: [Monad M] -> [MonadRng M] -> M F
    fromUInt64: UInt64 -> F

class PrimeField (F: Type)
  extends
    Field F
  where
    toNat: F -> Nat

class RootsOfUnity (F: Type) where
  MAX_ROU_SIZE: Nat
  ROU_FWD: Array F
  ROU_REV: Array F

class Algebra (F: Type) (R: Type)
  extends
    Ring R,
    HMul R F R
  where
    ofBase: F -> R
    ofBasis: Nat -> F -> R
    hMul := λ r f => (r * (ofBase f))

class ExtField (F R: Type) 
  extends
    Algebra F R
  where
    EXT_DEG: Nat
    ofSubelems : Array F -> R

class Algebraic (Elem ExtElem: Type) where
  prime_field: PrimeField Elem
  prime_rou: RootsOfUnity Elem
  ext_field: Field ExtElem
  ext_rou: RootsOfUnity ExtElem
  alg: Algebra Elem ExtElem
  ext: ExtField Elem ExtElem

@[always_inline]
instance [Algebraic Elem ExtElem] : PrimeField Elem := Algebraic.prime_field ExtElem

@[always_inline]
instance [Algebraic Elem ExtElem] : RootsOfUnity Elem := Algebraic.prime_rou ExtElem

@[always_inline]
instance [Algebraic Elem ExtElem] : Field ExtElem := Algebraic.ext_field Elem

@[always_inline]
instance [Algebraic Elem ExtElem] : RootsOfUnity ExtElem := Algebraic.ext_rou Elem

@[always_inline]
instance [Algebraic Elem ExtElem] : Algebra Elem ExtElem := Algebraic.alg

@[always_inline]
instance [Algebraic Elem ExtElem] : ExtField Elem ExtElem := Algebraic.ext

def polyEval [Ring Elem] (coeffs: Subarray Elem) (x: Elem): Elem
  := Id.run do
        let mut out: Elem := Ring.zero
        let mut x_pow: Elem := Ring.one
        for coeff in coeffs do
          out := out + x_pow * coeff
          x_pow := x_pow * x
        pure out

end Zkvm.Algebra.Classes
