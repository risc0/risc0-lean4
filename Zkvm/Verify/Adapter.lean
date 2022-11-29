/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import R0sy.Algebra.Field.BabyBear
import Zkvm.Circuit
import Zkvm.Verify.Classes

namespace Zkvm.Verify.Adapter

open Circuit
open Classes
open R0sy.Algebra
open R0sy.Algebra.Field

structure VerifyAdapter where
  po2: UInt32
  steps: UInt64
  out: Option (Array BabyBear.Elem)
  mix: Array BabyBear.Elem

def VerifyAdapter.new: VerifyAdapter := {
  po2 := 0,
  steps := 0,
  out := none,
  mix := #[]
}

def VerifyAdapter.execute [Monad M] [MonadStateOf VerifyAdapter M] [MonadReadIop M] (circuit: Circuit): M Unit
  := do let out <-
          (do let out <- MonadReadIop.readFields BabyBear.Elem circuit.outputSize
              return (some out))
        let po2 <-
          (do let po2 <- MonadReadIop.readU32s 1
              return po2[0]!)
        let steps := UInt64.ofNat (1 <<< (UInt32.toNat po2))
        let self: VerifyAdapter <- get
        set { self with
          po2,
          steps,
          out,
        }

def VerifyAdapter.accumulate [Monad M] [MonadStateOf VerifyAdapter M] [MonadReadIop M] (circuit: Circuit) (i := 0) (mix: Array BabyBear.Elem := #[]): M Unit
  := if i < circuit.mixSize
      then do let x: BabyBear.Elem <- Field.random
              VerifyAdapter.accumulate circuit (i + 1) (mix.push x)
      else do let self <- get
              set { self with mix }
termination_by _ => circuit.mixSize - i

instance [Monad M] [MonadStateOf VerifyAdapter M] [MonadReadIop M] [MonadCircuit M] : MonadVerifyAdapter M where
  getPo2
    := do let self <- get
          return self.po2
  execute := MonadCircuit.getCircuit >>= VerifyAdapter.execute
  accumulate := MonadCircuit.getCircuit >>= VerifyAdapter.accumulate
  verifyOutput := sorry

end Zkvm.Verify.Adapter
