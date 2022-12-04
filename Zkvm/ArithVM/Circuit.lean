/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.ArithVM.AST
import Zkvm.ArithVM.Taps

namespace Zkvm.ArithVM.Circuit

open R0sy.Algebra
open R0sy.Algebra.Field
open AST
open Taps


class Algebraic (Elem ExtElem: Type) where
  prime: PrimeField Elem
  rou: RootsOfUnity Elem
  ext: Field ExtElem
  alg: Algebra Elem ExtElem

instance [Algebraic Elem ExtElem] : PrimeField Elem := Algebraic.prime ExtElem

instance [Algebraic Elem ExtElem] : RootsOfUnity Elem := Algebraic.rou ExtElem

instance [Algebraic Elem ExtElem] : Field ExtElem := Algebraic.ext Elem

instance [Algebraic Elem ExtElem] : Algebra Elem ExtElem := Algebraic.alg

instance : Algebraic BabyBear.Elem BabyBear.ExtElem where
  prime := BabyBear.ElemPrimeField
  rou := BabyBear.ElemRootsOfUnity
  ext := BabyBear.ExtElemField
  alg := BabyBear.ElemExtElemAlgebra

instance : Algebraic Goldilocks.Elem Goldilocks.ExtElem where
  prime := Goldilocks.ElemPrimeField
  rou := Goldilocks.ElemRootsOfUnity
  ext := Goldilocks.ExtElemField
  alg := Goldilocks.ElemExtElemAlgebra


structure Circuit (Elem ExtElem: Type) where
  outputSize: Nat
  mixSize: Nat
  taps: TapSet
  polydef: PolyExtStepDef

def Circuit.poly_ext [Field Elem] [Field ExtElem] [Algebra Elem ExtElem] (self: Circuit Elem ExtElem) (mix: ExtElem) (u: Array ExtElem) (args: Array (Array Elem)): MixState ExtElem
  := PolyExtStepDef.run self.polydef mix u args

def riscv (taps: TapSet): Circuit R0sy.Algebra.Field.BabyBear.Elem R0sy.Algebra.Field.BabyBear.ExtElem where
  outputSize := 18
  mixSize := 36
  taps := taps
  polydef := {
    block := #[],       -- TODO!
    ret := { rep := 0 } -- TODO!
  }

end Zkvm.ArithVM.Circuit
