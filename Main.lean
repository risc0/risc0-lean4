/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm

def main : IO Unit
  := do IO.println s!"BabyBear has characteristic {R0sy.Algebra.Field.BabyBear.P.rep}"
        IO.println s!"Goldilocks has characteristic {R0sy.Algebra.Field.Goldilocks.P.rep}"
        -- TODO: Read a circuit and a seal, then invoke Zkvm.Verify.run_verify
