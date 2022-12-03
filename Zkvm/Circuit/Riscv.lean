/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra.Field.BabyBear
import Zkvm.Circuit

namespace Zkvm.Circuit.Riscv

open R0sy.Algebra.Field

def riscv: Circuit BabyBear.Elem BabyBear.ExtElem where
  outputSize := 18
  mixSize := 36
  taps := Inhabited.default -- TODO! sorry
  poly_ext mix u args := sorry

end Zkvm.Circuit.Riscv
