/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import R0sy.Algebra.Field.BabyBear
import Zkvm.Circuit
import Zkvm.Taps
import Zkvm.Verify.Classes

namespace Zkvm.Verify.Adapter

open Circuit
open Classes
open Taps
open R0sy.Algebra
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

def VerifyAdapter.execute (C: Type) [Monad M] [MonadStateOf (VerifyAdapter C) M] [MonadReadIop M] [CircuitInfo C]: M Unit
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

def VerifyAdapter.accumulate (C: Type) [Monad M] [MonadStateOf (VerifyAdapter C) M] [MonadReadIop M] [CircuitInfo C] (i := 0) (mix: Array BabyBear.Elem := #[]): M Unit
  := if i < CircuitInfo.mixSize C
      then do let x: BabyBear.Elem <- Field.random
              VerifyAdapter.accumulate C (i + 1) (mix.push x)
      else do let self <- get
              set { self with mix }
termination_by _ => CircuitInfo.mixSize C - i

instance [Monad M] [MonadStateOf (VerifyAdapter C) M] [MonadReadIop M] [CircuitInfo C] [TapsProvider C] : MonadVerifyAdapter M where
  getTaps
    := do let self <- get
          return TapsProvider.taps self.circuit
  getPo2
    := do let self <- get
          return self.po2
  execute := VerifyAdapter.execute C
  accumulate := VerifyAdapter.accumulate C
  verifyOutput := sorry

end Zkvm.Verify.Adapter
