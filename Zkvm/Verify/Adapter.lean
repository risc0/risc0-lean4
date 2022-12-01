/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import R0sy.Hash.Sha2
import R0sy.Lean.UInt32
import R0sy.Serial
import Zkvm.Circuit
import Zkvm.Verify.Classes

namespace Zkvm.Verify.Adapter

open R0sy.Algebra
open R0sy.Hash.Sha2
open R0sy.Lean.UInt32
open R0sy.Serial
open Circuit
open Classes

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

def VerifyAdapter.verifyOutput.toUInt32 [PrimeField Elem] (lo hi: Elem): UInt32 :=
  let lo' := PrimeField.toNat lo
  let hi' := PrimeField.toNat hi
  UInt32.ofNat (lo' ||| (hi' <<< 16))

def VerifyAdapter.verifyOutputAux [Monad M] [MonadExceptOf VerificationError M] [PrimeField Elem] (journal: Array UInt32) (output: Array Elem) (i: Nat := 0): M Unit
  := if h: i < journal.size
      then 
        let j := journal[i]
        let s := VerifyAdapter.verifyOutput.toUInt32 output[2 * i]! output[2 * i + 1]!
        if j != s
          then throw (VerificationError.JournalSealRootMismatch i s j)
          else VerifyAdapter.verifyOutputAux journal output (i + 1)
      else pure ()
termination_by _ => journal.size - i

def VerifyAdapter.verifyOutput [Monad M] [MonadExceptOf VerificationError M] [PrimeField Elem] (journal: Array UInt32) (output: Array Elem): M Unit
  := do let journal' <-
          (do let result_length_index := 16
              let output_len_lo := output[result_length_index]!
              let output_len_hi := output[result_length_index + 1]!
              let output_len := (VerifyAdapter.verifyOutput.toUInt32 output_len_lo output_len_hi).toNat
              let journal_len := journal.size * 4
              if journal_len != output_len
                then throw (VerificationError.SealJournalLengthMismatch output_len journal_len)
                else pure <| if journal.size <= 8 then journal else (Sha256.Digest.toSubarray (Sha256.hash_pod journal)))
        VerifyAdapter.verifyOutputAux journal' output

instance [Monad M] [MonadStateOf (VerifyAdapter Elem) M] [MonadExceptOf VerificationError M] [MonadReadIop M] [MonadCircuit Elem ExtElem M] [PrimeField Elem] : MonadVerifyAdapter M where
  getPo2
    := do let self <- get
          return self.po2
  execute
    := do let circuit: Circuit Elem ExtElem <- MonadCircuit.getCircuit
          VerifyAdapter.execute circuit
  accumulate
    := do let circuit: Circuit Elem ExtElem <- MonadCircuit.getCircuit
          VerifyAdapter.accumulate circuit
  verifyOutput journal
    := do let self <- get
          match self.out with
          | none => pure ()
          | some output => VerifyAdapter.verifyOutput journal output

end Zkvm.Verify.Adapter
