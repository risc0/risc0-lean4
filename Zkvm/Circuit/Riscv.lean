/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.Circuit
import Zkvm.Taps
import Zkvm.VM

namespace Zkvm.Circuit.Riscv

open R0sy.Algebra.Field
open Taps
open VM

def riscv (taps: TapSet): Circuit BabyBear.Elem BabyBear.ExtElem where
  outputSize := 18
  mixSize := 36
  taps := taps
  polydef := {
    block := #[],       -- TODO!
    ret := { rep := 0 } -- TODO!
  }

end Zkvm.Circuit.Riscv
