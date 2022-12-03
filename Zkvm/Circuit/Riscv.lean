/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra.Field.BabyBear
import Zkvm.Circuit
import Zkvm.Taps

namespace Zkvm.Circuit.Riscv

open R0sy.Algebra.Field
open Taps

def riscv (taps: TapSet): Circuit BabyBear.Elem BabyBear.ExtElem where
  outputSize := 18
  mixSize := 36
  taps := taps
  poly_ext mix u args := sorry

end Zkvm.Circuit.Riscv
