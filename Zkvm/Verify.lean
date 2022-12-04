/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.Circuit
import Zkvm.Constants
import Zkvm.MethodId
import Zkvm.Taps
import Zkvm.Verify.Classes
import Zkvm.Verify.Merkle
import Zkvm.Verify.Monad

namespace Zkvm.Verify

open R0sy.Algebra
open R0sy.Hash.Sha2
open Circuit
open Classes
open Merkle
open MethodId
open Taps


structure MerkleVerifiers where
  code: MerkleTreeVerifier
  data: MerkleTreeVerifier
  accum: MerkleTreeVerifier

def MerkleVerifiers.check_code_root (Elem ExtElem: Type) [Algebraic Elem ExtElem] [Monad.MonadVerify Elem ExtElem M] (code: MerkleTreeVerifier): M Unit
  := do let method_id <- MonadMethodId.getMethodId
        let po2 <- MonadVerifyAdapter.get_po2 Elem
        let which := po2 - Constants.MIN_CYCLES_PO2
        if which >= method_id.table.size then throw (VerificationError.MethodCycleError po2)
        if method_id.table[which]! != MerkleTreeVerifier.root code then throw VerificationError.MethodVerificationError
        pure ()

def MerkleVerifiers.new (Elem ExtElem: Type) [Algebraic Elem ExtElem] [Monad.MonadVerify Elem ExtElem M]: M MerkleVerifiers
  := do let circuit: Circuit Elem ExtElem <- MonadCircuit.getCircuit
        let domain <- MonadVerifyAdapter.get_domain Elem
        let code <- MerkleTreeVerifier.new domain (TapSet.groupSize circuit.taps RegisterGroup.Code) Constants.QUERIES
        MerkleVerifiers.check_code_root Elem ExtElem code
        let data <- MerkleTreeVerifier.new domain (TapSet.groupSize circuit.taps RegisterGroup.Data) Constants.QUERIES
        MonadVerifyAdapter.accumulate Elem
        let accum <- MerkleTreeVerifier.new domain (TapSet.groupSize circuit.taps RegisterGroup.Accum) Constants.QUERIES
        pure {
          code,
          data,
          accum
        }


def verify.enforce_max_cycles (Elem ExtElem: Type) [Algebraic Elem ExtElem] [Monad.MonadVerify Elem ExtElem M]: M Unit
  := do let po2 <- MonadVerifyAdapter.get_po2 Elem
        if po2 > Constants.MAX_CYCLES_PO2 then panic s!"po2:{po2} > Constants.MAX_CYCLES_PO2:{Constants.MAX_CYCLES_PO2}"
        pure ()

structure CheckVerifier (ExtElem: Type) where
  check: MerkleTreeVerifier
  coeff_u: Array ExtElem
  result: ExtElem

def CheckVerifier.evaluate (Elem ExtElem: Type) [Algebraic Elem ExtElem] [Monad.MonadVerify Elem ExtElem M] (regs: RegIter) (z: ExtElem) (coeff_u: Array ExtElem): M (Array ExtElem)
  := do let po2 <- MonadVerifyAdapter.get_po2 Elem
        let back_one := (@RootsOfUnity.ROU_REV Elem _)[po2]!
        throw (VerificationError.Sorry "CheckVerifier.evaluate")

def CheckVerifier.new (Elem ExtElem: Type) [Algebraic Elem ExtElem] [Monad.MonadVerify Elem ExtElem M]: M (CheckVerifier ExtElem)
  := do let poly_mix <- @Field.random ExtElem _ M _ _
        let check
          <- do let domain <- MonadVerifyAdapter.get_domain Elem
                MerkleTreeVerifier.new domain Constants.CHECK_SIZE Constants.QUERIES
        let z <- @Field.random ExtElem _ M _ _
        -- Read the U coeffs + commit their hash
        let circuit: Circuit Elem ExtElem <- MonadCircuit.getCircuit
        let num_taps := TapSet.tapSize circuit.taps
        let coeff_u <- MonadReadIop.readFields ExtElem (num_taps + Constants.CHECK_SIZE)
        MonadReadIop.commit (Sha256.hash_pod coeff_u)
        -- Now convert to evaluated values
        let eval_u <- CheckVerifier.evaluate Elem ExtElem (TapSet.regIter circuit.taps) z coeff_u
        -- Compute the core polynomial
        let out <- MonadVerifyAdapter.get_out
        let mix <- MonadVerifyAdapter.get_mix
        let result := (circuit.poly_ext poly_mix eval_u #[out, mix]).tot
        pure {
          check,
          coeff_u,
          result
        }

def verify (Elem ExtElem: Type) [Algebraic Elem ExtElem] [Monad.MonadVerify Elem ExtElem M] (journal: Array UInt32): M Unit
  := do -- Initialize the adapter
        MonadVerifyAdapter.execute Elem
        MonadVerifyAdapter.verifyOutput Elem journal
        -- Enforce constraints on cycle count
        verify.enforce_max_cycles Elem ExtElem
        -- Get the Merkle trees
        let merkle_verifiers <- MerkleVerifiers.new Elem ExtElem
        -- Begin the check process
        let check_verifier <- CheckVerifier.new Elem ExtElem
        -- Sorry! We have not implemented the rest of the logic yet ;)
        throw (VerificationError.Sorry "Zkvm.Verify.verify")


def run_verify [Algebraic Elem ExtElem] (circuit: Circuit Elem ExtElem) (method_id: MethodId) (journal seal: Array UInt32)
  := Monad.VerifyContext.run circuit method_id seal (verify Elem ExtElem journal)

end Zkvm.Verify
