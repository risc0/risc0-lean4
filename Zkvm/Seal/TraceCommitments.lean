/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.ArithVM.Circuit
import Zkvm.ArithVM.Taps
import Zkvm.MethodId
import Zkvm.Seal.Header
import Zkvm.Verify.Error
import Zkvm.Verify.Merkle
import Zkvm.Verify.ReadIop

namespace Zkvm.Seal.TraceCommitments

open R0sy.Algebra
open Zkvm.ArithVM.Circuit
open Zkvm.ArithVM.Taps
open Zkvm.MethodId
open Zkvm.Verify.Error
open Zkvm.Verify.Merkle
open Zkvm.Verify.ReadIop


structure TraceCommitments (Elem: Type) where
  code_merkle: MerkleTreeVerifier
  data_merkle: MerkleTreeVerifier
  accum_merkle: MerkleTreeVerifier
  mix: Array Elem

def check_code_root
    [Monad M]
    [MonadExceptOf VerificationError M]
    [RootsOfUnity Elem]
    (method_id: MethodId)
    (header: Header.Header Elem)
    (code_merkle: MerkleTreeVerifier)
    : M Unit
  := do let which := header.po2 - Constants.MIN_CYCLES_PO2
        if which >= method_id.table.size then throw (VerificationError.MethodCycleError header.po2)
        if method_id.table[which]! != MerkleTreeVerifier.root code_merkle then throw VerificationError.MethodVerificationError
        pure ()

def get_mix [Monad M] [MonadReadIop M] (circuit: Circuit): M (Array circuit.field.Elem)
  := do let mut mix := Array.mkEmpty circuit.mix_size
        for _ in [0:circuit.mix_size] do
          let x <- Field.random
          mix := mix.push x
        pure mix

def read_and_commit
    [Monad M]
    [MonadReadIop M]
    [MonadExceptOf VerificationError M]
    (circuit: Circuit)
    (header: Header.Header circuit.field.Elem)
    (method_id: MethodId)
    : M (TraceCommitments circuit.field.Elem)
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
