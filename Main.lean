/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import ZkvmVerify

def main : IO Unit := do
  IO.println s!"BabyBear has characteristic {ZkvmVerify.Algebra.Field.BabyBear.P.rep}"
  IO.println s!"Goldilocks has characteristic {ZkvmVerify.Algebra.Field.Goldilocks.P.rep}"
