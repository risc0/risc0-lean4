/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import Zkvm.Circuit
import Zkvm.Verify.Classes

namespace Zkvm.Verify.Adapter

open Circuit
open Classes
open R0sy.Algebra

structure VerifyAdapter (Elem: Type) where
  po2: UInt32
  steps: UInt64
  out: Option (Array Elem)
  mix: Array Elem

def VerifyAdapter.new: VerifyAdapter Elem := {
  po2 := 0,
  steps := 0,
  out := none,
  mix := #[]
}

def VerifyAdapter.execute [Monad M] [MonadStateOf (VerifyAdapter Elem) M] [MonadReadIop M] [Field Elem] (circuit: Circuit Elem ExtElem): M Unit
  := do let out <-
          (do let out <- MonadReadIop.readFields Elem circuit.outputSize
              return (some out))
        let po2 <-
          (do let po2 <- MonadReadIop.readU32s 1
              return po2[0]!)
        let steps := UInt64.ofNat (1 <<< (UInt32.toNat po2))
        let self: VerifyAdapter Elem <- get
        set { self with
          po2,
          steps,
          out,
        }

def VerifyAdapter.accumulate [Monad M] [MonadStateOf (VerifyAdapter Elem) M] [MonadReadIop M] [Field Elem] (circuit: Circuit Elem ExtElem) (i := 0) (mix: Array Elem := #[]): M Unit
  := if i < circuit.mixSize
      then do let x: Elem <- Field.random
              VerifyAdapter.accumulate circuit (i + 1) (mix.push x)
      else do let self <- get
              set { self with mix }
termination_by _ => circuit.mixSize - i

instance [Monad M] [MonadStateOf (VerifyAdapter Elem) M] [MonadReadIop M] [MonadCircuit Elem ExtElem M] [Field Elem] : MonadVerifyAdapter M where
  getPo2
    := do let self <- get
          return self.po2
  execute
    := do let circuit: Circuit Elem ExtElem <- MonadCircuit.getCircuit
          VerifyAdapter.execute circuit
  accumulate
    := do let circuit: Circuit Elem ExtElem <- MonadCircuit.getCircuit
          VerifyAdapter.accumulate circuit
  verifyOutput := sorry

end Zkvm.Verify.Adapter
