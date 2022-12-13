/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.ArithVM.Circuit
import Zkvm.ArithVM.Taps
import Zkvm.Seal.Header
import Zkvm.Verify.Classes
import Zkvm.Verify.Merkle
import Zkvm.Verify.Monad

namespace Zkvm.Seal.TraceCommitments

open R0sy.Algebra
open Zkvm.ArithVM.Circuit
open Zkvm.ArithVM.Taps
open Zkvm.Verify.Classes
open Zkvm.Verify.Merkle
open Zkvm.Verify.Monad

structure TraceVerifier (Elem: Type) where
  code_merkle: MerkleTreeVerifier
  data_merkle: MerkleTreeVerifier
  accum_merkle: MerkleTreeVerifier
  mix: Array Elem

def check_code_root [MonadVerify M] [RootsOfUnity Elem] (header: Header.Header Elem) (code_merkle: MerkleTreeVerifier): M Unit
  := do let method_id <- MonadMethodId.getMethodId
        let which := header.po2 - Constants.MIN_CYCLES_PO2
        if which >= method_id.table.size then throw (VerificationError.MethodCycleError header.po2)
        if method_id.table[which]! != MerkleTreeVerifier.root code_merkle then throw VerificationError.MethodVerificationError
        pure ()

def compute_mix [Monad M] [MonadReadIop M] [Field Elem] (circuit: Circuit): M (Array Elem)
  := do let mut mix: Array Elem := Array.mkEmpty circuit.mix_size
        for _ in [0:circuit.mix_size] do
          let x: Elem <- Field.random
          mix := mix.push x
        pure mix

def read_and_commit [MonadVerify M] [Field Elem] [RootsOfUnity Elem] (header: Header.Header Elem): M (TraceVerifier Elem)
  := do let circuit <- MonadCircuit.getCircuit
        let code_merkle <- MerkleTreeVerifier.read_and_commit header.domain (TapSet.groupSize circuit.taps RegisterGroup.Code) Constants.QUERIES
        check_code_root header code_merkle
        let data_merkle <- MerkleTreeVerifier.read_and_commit header.domain (TapSet.groupSize circuit.taps RegisterGroup.Data) Constants.QUERIES
        let mix <- compute_mix circuit
        let accum_merkle <- MerkleTreeVerifier.read_and_commit header.domain (TapSet.groupSize circuit.taps RegisterGroup.Accum) Constants.QUERIES
        pure {
          code_merkle,
          data_merkle,
          accum_merkle,
          mix
        }

end Zkvm.Seal.TraceCommitments
