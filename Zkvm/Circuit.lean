/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.Taps
import Zkvm.VM

namespace Zkvm.Circuit

open R0sy.Algebra
open Taps
open VM

structure Circuit (Elem ExtElem: Type) where
  outputSize: Nat
  mixSize: Nat
  taps: TapSet
  polydef: PolyExtStepDef

def Circuit.poly_ext [Field Elem] [Field ExtElem] [Algebra Elem ExtElem] (self: Circuit Elem ExtElem) (mix: ExtElem) (u: Array ExtElem) (args: Array (Array Elem)): MixState ExtElem
  := PolyExtStepDef.run self.polydef mix u args

end Zkvm.Circuit
