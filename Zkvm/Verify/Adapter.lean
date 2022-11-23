/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra.Field.BabyBear
import Zkvm.Circuit

namespace Zkvm.Verify.Adapter

open Circuit
open R0sy.Algebra.Field

structure VerifyAdapter (C: Type) where
  circuit: C
  po2: UInt32
  steps: UInt64
  out: Option BabyBear.Elem
  mix: Array BabyBear.Elem

def VerifyAdapter.new (circuit: C): VerifyAdapter C := {
  circuit,
  po2 := 0,
  steps := 0,
  out := none,
  mix := #[]
}

def VerifyAdapter.taps [TapsProvider C] (adapter: VerifyAdapter C): TapSet := TapsProvider.taps adapter.circuit

-- TODO: execute

-- TODO: accumulate

end Zkvm.Verify.Adapter
