/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.ArithVM
import Zkvm.Taps

namespace Zkvm.Circuit

open R0sy.Algebra
open ArithVM
open Taps

structure Circuit (Elem ExtElem: Type) where
  outputSize: Nat
  mixSize: Nat
  taps: TapSet
  polydef: PolyExtStepDef

def Circuit.poly_ext [Field Elem] [Field ExtElem] [Algebra Elem ExtElem] (self: Circuit Elem ExtElem) (mix: ExtElem) (u: Array ExtElem) (args: Array (Array Elem)): MixState ExtElem
  := PolyExtStepDef.run self.polydef mix u args

end Zkvm.Circuit
