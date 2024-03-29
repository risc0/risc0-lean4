/-
 Copyright 2023 RISC Zero, Inc.

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
-/

import R0sy
import Zkvm.Algebra.Classes
import Zkvm.ArithVM.Circuit
import Zkvm.Verify.Error
import Zkvm.Verify.ReadIop

namespace Zkvm.Seal.Header

open R0sy.Hash
open R0sy.Lean.Nat
open R0sy.Lean.UInt32
open R0sy.Serial
open Zkvm.Algebra.Classes
open Zkvm.ArithVM.Circuit
open Zkvm.Verify.Error
open Zkvm.Verify.ReadIop

structure Header (Elem: Type) where
  po2: Nat
  size: Nat
  domain: Nat
  fri_gen: Elem
  back_one: Elem
  output: Array Elem
  deserialized_output: Array UInt32


def read [Monad M] [MonadReadIop M] (circuit: Circuit): M (Header circuit.field.Elem)
  := do let output <- MonadReadIop.readFields circuit.field.Elem circuit.output_size
        let mut deserialized_output := Array.mkEmpty (output.size / 2)
        for i in [0:output.size / 2] do
          let hi := PrimeField.toNat output[2 * i + 1]!
          let lo := PrimeField.toNat output[2 * i]!
          deserialized_output := deserialized_output.push ((hi <<< 16) ||| lo).toUInt32
        let po2 <- MonadReadIop.readU32s 1 >>= (fun x => pure <| x[0]!.toNat)
        let size := 1 <<< po2
        let domain := Constants.INV_RATE * size
        let fri_gen := RootsOfUnity.ROU_FWD[Nat.log2_ceil (domain)]!
        let back_one := RootsOfUnity.ROU_REV[po2]!
        pure {
          po2,
          size,
          domain,
          fri_gen,
          back_one,
          output,
          deserialized_output
        }

def verify_journal_size [Monad M] [MonadExceptOf VerificationError M] [PrimeField Elem] (self: Header Elem) (journal: Array UInt32): M Unit
  := do let output_len_idx := self.deserialized_output.size - 1
        let output_len := self.deserialized_output[output_len_idx]!.toNat
        let journal_len := journal.size * 4
        if output_len != journal_len
          -- Returns error if there's a mismatch between the length of the journal and the purported journal-length on the seal
          then throw (VerificationError.JournalLengthMismatch output_len journal_len)

def verify_journal (D: Type) [Monad M] [MonadExceptOf VerificationError M] [PrimeField Elem] [Hash D] (self: Header Elem) (journal: Array UInt32): M Unit
  := do verify_journal_size self journal
        let journal
          := if journal.size <= SerialUInt32.words D
              then journal
              else SerialUInt32.toUInt32Words (Hash.hash_pod journal: D)
        for i in [0:journal.size] do
          let s := self.deserialized_output[i]!
          let j := journal[i]!
          -- Returns error if there's a mismatch between the journal on the receipt and the purported journal-hash on the seal (TODO confirm logic)
          if j != s then throw (VerificationError.JournalHashMismatch i s j)
        pure ()

end Zkvm.Seal.Header
