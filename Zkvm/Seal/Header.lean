/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.ArithVM.Circuit
import Zkvm.Constants
import Zkvm.Verify.Classes

namespace Zkvm.Seal.Header

open R0sy.Algebra
open R0sy.Hash.Sha2
open R0sy.Lean.Nat
open R0sy.Lean.UInt32
open R0sy.Serial
open ArithVM.Circuit
open Verify.Classes

structure Header (Elem: Type) where
  po2: Nat
  size: Nat
  domain: Nat
  back_one: Elem
  journal: Array Elem

def read [Monad M] [MonadReadIop M] [Field Elem] [RootsOfUnity Elem] (circuit: Circuit): M (Header Elem)
  := do let journal <- MonadReadIop.readFields Elem circuit.output_size
        let po2 <- MonadReadIop.readU32s 1 >>= (fun x => pure <| x[0]!.toNat)
        let size := 1 <<< po2
        let domain := Constants.INV_RATE * size
        let back_one := RootsOfUnity.ROU_REV[po2]!
        pure {
          po2,
          size,
          domain,
          back_one,
          journal,
        }

def verify_journal [Monad M] [MonadExceptOf VerificationError M] [PrimeField Elem] (self: Header Elem) (journal: Array UInt32): M Unit
  := do let deserialize (lo hi: Elem): UInt32 :=
          let lo' := PrimeField.toNat lo
          let hi' := PrimeField.toNat hi
          UInt32.ofNat (lo' ||| (hi' <<< 16))
        let journal <- do
          let output_len :=
            let result_length_index := 16
            let lo := self.journal[result_length_index]!
            let hi := self.journal[result_length_index + 1]!
            (deserialize lo hi).toNat
          let journal_len := journal.size * 4
          if journal_len != output_len then throw (VerificationError.SealJournalLengthMismatch output_len journal_len)
          else if journal.size <= 8 then pure journal
          else pure (Sha256.Digest.toSubarray (Sha256.hash_pod journal))
        for i in [0:journal.size] do
          let lo := self.journal[2 * i]!
          let hi := self.journal[2 * i + 1]!
          let s := deserialize lo hi
          let j := journal[i]!
          if j != s then throw (VerificationError.JournalSealRootMismatch i s j)
        pure ()

end Zkvm.Seal.Header
