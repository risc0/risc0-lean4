/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.ArithVM.Circuit
import Zkvm.Constants
import Zkvm.MethodId
import Zkvm.ArithVM.Taps
import Zkvm.Verify.Classes
import Zkvm.Verify.Merkle
import Zkvm.Verify.Monad

namespace Zkvm.Verify

open R0sy.Algebra
open R0sy.Algebra.Poly
open R0sy.Hash.Sha2
open ArithVM.Circuit
open ArithVM.Taps
open Classes
open Merkle
open MethodId


structure MerkleVerifiers where
  code: MerkleTreeVerifier
  data: MerkleTreeVerifier
  accum: MerkleTreeVerifier

def MerkleVerifiers.check_code_root [Monad.MonadVerify M Elem ExtElem] [Algebraic Elem ExtElem] (code: MerkleTreeVerifier): M Unit
  := do let method_id <- MonadMethodId.getMethodId
        let po2 <- MonadVerifyAdapter.get_po2
        let which := po2 - Constants.MIN_CYCLES_PO2
        if which >= method_id.table.size then throw (VerificationError.MethodCycleError po2)
        if method_id.table[which]! != MerkleTreeVerifier.root code then throw VerificationError.MethodVerificationError
        pure ()

def MerkleVerifiers.new [Monad.MonadVerify M Elem ExtElem] [Algebraic Elem ExtElem]: M MerkleVerifiers
  := do let circuit <- MonadCircuit.getCircuit
        let domain <- MonadVerifyAdapter.get_domain
        let code <- MerkleTreeVerifier.new domain (TapSet.groupSize circuit.taps RegisterGroup.Code) Constants.QUERIES
        MerkleVerifiers.check_code_root code
        let data <- MerkleTreeVerifier.new domain (TapSet.groupSize circuit.taps RegisterGroup.Data) Constants.QUERIES
        MonadVerifyAdapter.accumulate
        let accum <- MerkleTreeVerifier.new domain (TapSet.groupSize circuit.taps RegisterGroup.Accum) Constants.QUERIES
        pure {
          code,
          data,
          accum
        }


structure CheckVerifier (ExtElem: Type) where
  check: MerkleTreeVerifier
  coeff_u: Array ExtElem
  result: ExtElem

def CheckVerifier.evaluate [Monad.MonadVerify M Elem ExtElem] [Algebraic Elem ExtElem] (regs: RegIter) (z: ExtElem) (coeff_u: Array ExtElem): M (Array ExtElem)
  := do let circuit <- MonadCircuit.getCircuit
        let po2 <- MonadVerifyAdapter.get_po2
        let back_one := (@RootsOfUnity.ROU_REV Elem _)[po2]!
        let mut cur_pos := 0
        let mut eval_u: Array ExtElem := Array.mkEmpty (TapSet.tapSize circuit.taps)
        for reg in regs do
          let reg_size := RegRef.size reg
          for i in [0:reg_size] do
            let x := z * Algebra.ofBase (back_one ^ (RegRef.back reg i))
            let poly := Poly.ofSubarray (Array.toSubarray coeff_u cur_pos (cur_pos + reg_size))
            eval_u := eval_u.push (Poly.eval poly x)
          cur_pos := cur_pos + reg_size
        pure eval_u

def CheckVerifier.new [Monad.MonadVerify M Elem ExtElem] [Algebraic Elem ExtElem]: M (CheckVerifier ExtElem)
  := do let poly_mix <- @Field.random ExtElem _ M _ _
        let check
          <- do let domain <- MonadVerifyAdapter.get_domain
                MerkleTreeVerifier.new domain Constants.CHECK_SIZE Constants.QUERIES
        let z <- @Field.random ExtElem _ M _ _
        -- Read the U coeffs + commit their hash
        let circuit <- MonadCircuit.getCircuit
        let num_taps := TapSet.tapSize circuit.taps
        let coeff_u <- MonadReadIop.readFields ExtElem (num_taps + Constants.CHECK_SIZE)
        MonadReadIop.commit (Sha256.hash_pod coeff_u)
        -- Now convert to evaluated values
        let eval_u <- CheckVerifier.evaluate (TapSet.regIter circuit.taps) z coeff_u
        -- Compute the core polynomial
        let out <- MonadVerifyAdapter.get_out
        let mix <- MonadVerifyAdapter.get_mix
        let result := (circuit.poly_ext poly_mix eval_u #[out, mix]).tot
        pure {
          check,
          coeff_u,
          result
        }


def verify.enforce_max_cycles [Monad.MonadVerify M Elem ExtElem] [Algebraic Elem ExtElem]: M Unit
  := do let po2 <- MonadVerifyAdapter.get_po2
        if po2 > Constants.MAX_CYCLES_PO2 then panic s!"po2:{po2} > Constants.MAX_CYCLES_PO2:{Constants.MAX_CYCLES_PO2}"
        pure ()

def verify (journal: Array UInt32) [Monad.MonadVerify M Elem ExtElem] [Algebraic Elem ExtElem]: M Unit
  := do -- Initialize the adapter
        MonadVerifyAdapter.execute
        MonadVerifyAdapter.verifyOutput journal
        -- Enforce constraints on cycle count
        verify.enforce_max_cycles
        -- Get the Merkle trees
        let merkle_verifiers <- MerkleVerifiers.new
        -- Begin the check process
        let check_verifier <- CheckVerifier.new
        -- Sorry! We have not implemented the rest of the logic yet ;)
        throw (VerificationError.Sorry "Zkvm.Verify.verify")


def run_verify [Algebraic Elem ExtElem] (circuit: Circuit Elem ExtElem) (method_id: MethodId) (journal seal: Array UInt32)
  := Monad.VerifyContext.run circuit method_id seal (verify journal)

end Zkvm.Verify
