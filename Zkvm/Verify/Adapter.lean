/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra.Field.BabyBear
import Zkvm.Circuit
import Zkvm.Verify.Classes

namespace Zkvm.Verify.Adapter

open Circuit
open Classes
open R0sy.Algebra.Field

structure VerifyAdapter (C: Type) where
  circuit: C
  po2: UInt32
  steps: UInt64
  out: Option (Array BabyBear.Elem)
  mix: Array BabyBear.Elem

def VerifyAdapter.new (circuit: C): VerifyAdapter C := {
  circuit,
  po2 := 0,
  steps := 0,
  out := none,
  mix := #[]
}

def VerifyAdapter.taps [TapsProvider C] (adapter: VerifyAdapter C): TapSet := TapsProvider.taps adapter.circuit

def VerifyAdapter.execute [CircuitInfo C] [Monad M] [MonadStateOf (VerifyAdapter C) M] [MonadReadIop M]: M Unit
  := do let out <-
          (do let out <- MonadReadIop.readFields BabyBear.Elem (CircuitInfo.outputSize C)
              return (some out))
        let po2 <-
          (do let po2 <- MonadReadIop.readU32s 1
              return po2[0]!)
        let steps := UInt64.ofNat (1 <<< (UInt32.toNat po2))
        let self: (VerifyAdapter C) <- get
        set { self with
          po2,
          steps,
          out,
        }

-- TODO: accumulate

end Zkvm.Verify.Adapter
