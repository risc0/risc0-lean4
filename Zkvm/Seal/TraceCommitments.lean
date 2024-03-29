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
import Zkvm.ArithVM.Taps
import Zkvm.MethodId
import Zkvm.Seal.Header
import Zkvm.Verify.Error
import Zkvm.Verify.Merkle
import Zkvm.Verify.ReadIop

namespace Zkvm.Seal.TraceCommitments

open R0sy.Hash
open Zkvm.Algebra.Classes
open Zkvm.ArithVM.Circuit
open Zkvm.ArithVM.Taps
open Zkvm.MethodId
open Zkvm.Verify.Error
open Zkvm.Verify.Merkle
open Zkvm.Verify.ReadIop


structure TraceCommitments (D Elem: Type) where
  code_merkle: MerkleTreeVerifier D
  data_merkle: MerkleTreeVerifier D
  accum_merkle: MerkleTreeVerifier D
  mix: Array Elem

def check_code_root
    [Monad M]
    [MonadExceptOf VerificationError M]
    [Hash D]
    [RootsOfUnity Elem]
    (method_id: MethodId D)
    (header: Header.Header Elem)
    (code_merkle: MerkleTreeVerifier D)
    : M Unit
  := do let which := header.po2 - Constants.MIN_CYCLES_PO2
        -- Returns error if zkvm execution was shorter than min PO2
        if which >= method_id.table.size then throw (VerificationError.TooFewCycles header.po2)
        -- Returns error if Merkle commitment for control tree (aka code tree) mismatches with the appropriate row of MethodID table
        if method_id.table[which]! != MerkleTreeVerifier.root code_merkle then throw VerificationError.MethodIDMismatch
        pure ()

def get_mix [Monad M] [MonadReadIop M] (circuit: Circuit): M (Array circuit.field.Elem)
  := do let mut mix := Array.mkEmpty circuit.mix_size
        for _ in [0:circuit.mix_size] do
          let x <- Field.random
          mix := mix.push x
        pure mix

def read_and_commit
    (D: Type)
    [Monad M]
    [MonadReadIop M]
    [MonadCommitIop D M]
    [MonadExceptOf VerificationError M]
    [Hash D]
    (circuit: Circuit)
    (header: Header.Header circuit.field.Elem)
    (method_id: MethodId D)
    : M (TraceCommitments D circuit.field.Elem)
  := do let code_merkle <- MerkleTreeVerifier.read_and_commit header.domain (TapSet.groupSize circuit.taps RegisterGroup.Code) Constants.QUERIES
        check_code_root method_id header code_merkle
        let data_merkle <- MerkleTreeVerifier.read_and_commit header.domain (TapSet.groupSize circuit.taps RegisterGroup.Data) Constants.QUERIES
        let mix <- get_mix circuit
        let accum_merkle <- MerkleTreeVerifier.read_and_commit header.domain (TapSet.groupSize circuit.taps RegisterGroup.Accum) Constants.QUERIES
        pure {
          code_merkle,
          data_merkle,
          accum_merkle,
          mix
        }

end Zkvm.Seal.TraceCommitments
