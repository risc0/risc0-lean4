/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.ArithVM.Circuit
import Zkvm.Constants
import Zkvm.Verify.Classes

namespace Zkvm.Verify.Adapter

open R0sy.Algebra
open R0sy.Hash.Sha2
open R0sy.Lean.Nat
open R0sy.Lean.UInt32
open R0sy.Serial
open ArithVM.Circuit
open Classes


structure VerifyAdapter (Elem: Type) where
  mix: Array Elem

def VerifyAdapter.new [Field Elem]: VerifyAdapter Elem := {
  mix := #[]
}

def VerifyAdapter.accumulate [Monad M] [MonadStateOf (VerifyAdapter Elem) M] [MonadReadIop M] [Field Elem] (circuit: Circuit Elem ExtElem) (i := 0) (mix: Array Elem := #[]): M Unit
  := if i < circuit.mix_size
      then do let x: Elem <- Field.random
              VerifyAdapter.accumulate circuit (i + 1) (mix.push x)
      else do let self <- get
              set { self with mix }
termination_by _ => circuit.mix_size - i


instance [Monad M] [MonadStateOf (VerifyAdapter Elem) M] [MonadExceptOf VerificationError M] [MonadReadIop M] [MonadCircuit M Elem ExtElem] [PrimeField Elem] [RootsOfUnity Elem] : MonadVerifyAdapter M Elem where
  get_mix
    := do let self <- get
          pure self.mix
  accumulate
    := do let circuit: Circuit Elem ExtElem <- MonadCircuit.getCircuit
          VerifyAdapter.accumulate circuit

end Zkvm.Verify.Adapter
