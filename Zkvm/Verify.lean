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
open R0sy.Serial
open ArithVM.Circuit
open ArithVM.Taps
open Classes
open Merkle
open MethodId

structure MerkleVerifiers where
  code_merkle: MerkleTreeVerifier
  data_merkle: MerkleTreeVerifier
  accum_merkle: MerkleTreeVerifier

def MerkleVerifiers.check_code_root [Monad.MonadVerify M Elem ExtElem] [Algebraic Elem ExtElem] (code_merkle: MerkleTreeVerifier): M Unit
  := do let method_id <- MonadMethodId.getMethodId
        let po2 <- MonadVerifyAdapter.get_po2
        let which := po2 - Constants.MIN_CYCLES_PO2
        if which >= method_id.table.size then throw (VerificationError.MethodCycleError po2)
        if method_id.table[which]! != MerkleTreeVerifier.root code_merkle then throw VerificationError.MethodVerificationError
        pure ()

def MerkleVerifiers.new [Monad.MonadVerify M Elem ExtElem] [Algebraic Elem ExtElem]: M MerkleVerifiers
  := do let circuit <- MonadCircuit.getCircuit
        let domain <- MonadVerifyAdapter.get_domain
        let code_merkle <- MerkleTreeVerifier.new domain (TapSet.groupSize circuit.taps RegisterGroup.Code) Constants.QUERIES
        MerkleVerifiers.check_code_root code_merkle
        let data_merkle <- MerkleTreeVerifier.new domain (TapSet.groupSize circuit.taps RegisterGroup.Data) Constants.QUERIES
        MonadVerifyAdapter.accumulate
        let accum_merkle <- MerkleTreeVerifier.new domain (TapSet.groupSize circuit.taps RegisterGroup.Accum) Constants.QUERIES
        pure {
          code_merkle,
          data_merkle,
          accum_merkle
        }


structure CheckVerifier (ExtElem: Type) where
  check_merkle: MerkleTreeVerifier
  mix: ExtElem
  combo_u: Array ExtElem

def CHECK_SIZE Elem ExtElem [ExtField Elem ExtElem] := Constants.INV_RATE * (ExtField.EXT_DEG Elem ExtElem)

def CheckVerifier.evaluate [Monad.MonadVerify M Elem ExtElem] [Algebraic Elem ExtElem] (regs: RegIter) (z: ExtElem) (coeff_u: Array ExtElem): M (Array ExtElem)
  := do let circuit <- MonadCircuit.getCircuit
        let po2 <- MonadVerifyAdapter.get_po2
        let back_one: Elem := RootsOfUnity.ROU_REV[po2]!
        let mut cur_pos := 0
        let mut eval_u: Array ExtElem := Array.mkEmpty (TapSet.tapSize circuit.taps)
        for reg in regs do
          let reg_size := RegRef.size reg
          for i in [0:reg_size] do
            let x := z * Algebra.ofBase (back_one ^ (RegRef.back reg i))
            let poly: Poly ExtElem := Poly.ofSubarray (coeff_u.toSubarray cur_pos (cur_pos + reg_size))
            let fx := Poly.eval poly x
            eval_u := eval_u.push fx
          cur_pos := cur_pos + reg_size
        pure eval_u

def CheckVerifier.new [Monad.MonadVerify M Elem ExtElem] [Algebraic Elem ExtElem]: M (CheckVerifier ExtElem)
  := do let poly_mix: ExtElem <- Field.random
        let check_merkle
          <- do let domain <- MonadVerifyAdapter.get_domain
                MerkleTreeVerifier.new domain (CHECK_SIZE Elem ExtElem) Constants.QUERIES
        let z: ExtElem <- Field.random
        -- Read the U coeffs + commit their hash
        let circuit <- MonadCircuit.getCircuit
        let num_taps := TapSet.tapSize circuit.taps
        let coeff_u <- MonadReadIop.readFields ExtElem (num_taps + CHECK_SIZE Elem ExtElem)
        let hash_u := Sha256.hash_pod coeff_u
        MonadReadIop.commit hash_u
        -- Now convert to evaluated values
        let eval_u <- CheckVerifier.evaluate (TapSet.regIter circuit.taps) z coeff_u
        -- Compute the core polynomial
        let out <- MonadVerifyAdapter.get_out
        let mix <- MonadVerifyAdapter.get_mix
        let result := (circuit.poly_ext poly_mix eval_u #[out, mix]).tot
        -- Generate the check polynomial
        let ext0: ExtElem := Algebra.ofBasis 0 (Ring.one : Elem)
        let ext1: ExtElem := Algebra.ofBasis 1 (Ring.one : Elem)
        let ext2: ExtElem := Algebra.ofBasis 2 (Ring.one : Elem)
        let ext3: ExtElem := Algebra.ofBasis 3 (Ring.one : Elem)
        let remap := #[0, 2, 1, 3]
        let mut check: ExtElem := Ring.zero
        for i in [0:4] do
          let rmi := remap[i]!
          let zpi := z ^ i
          check := check + ext0 * coeff_u[num_taps + rmi + 0]! * zpi
          check := check + ext1 * coeff_u[num_taps + rmi + 4]! * zpi
          check := check + ext2 * coeff_u[num_taps + rmi + 8]! * zpi
          check := check + ext3 * coeff_u[num_taps + rmi + 12]! * zpi
        let size <- MonadVerifyAdapter.get_size
        check := check * ((Ring.ofNat 3 * z) ^ size - Ring.one)
        if check != result then throw (VerificationError.InvalidCheck (ToString.toString result) (ToString.toString check))
        -- Set the mix value
        let mix: ExtElem <- Field.random
        -- Make the mixed polynomials
        let mut combo_u: Array ExtElem := Array.mkArray (circuit.taps.tot_combo_backs.toNat + 1) Ring.zero
        let mut cur_mix: ExtElem := Ring.one
        let mut cur_pos := 0
        let mut tap_mix_pows: Array ExtElem := Array.mkEmpty circuit.taps.reg_count.toNat
        for reg in (TapSet.regIter circuit.taps) do
          let reg_size := RegRef.size reg
          for i in [0:reg_size] do
            let idx := circuit.taps.combo_begin[reg.combo_id]!.toNat + i
            let val := combo_u[idx]! + cur_mix * coeff_u[cur_pos + i]!
            combo_u := Array.set! combo_u idx val
          tap_mix_pows := tap_mix_pows.push cur_mix
          cur_mix := cur_mix * mix
          cur_pos := cur_pos + reg_size
        -- Handle check group
        let mut check_mix_pows := Array.mkEmpty (CHECK_SIZE Elem ExtElem)
        for _ in [0:CHECK_SIZE Elem ExtElem] do
          let idx := circuit.taps.tot_combo_backs.toNat
          let val := combo_u[idx]! + cur_mix * coeff_u[cur_pos]!
          combo_u := Array.set! combo_u idx val
          cur_pos := cur_pos + 1
          check_mix_pows := check_mix_pows.push cur_mix
          cur_mix := cur_mix * mix
        pure {
          check_merkle,
          mix,
          combo_u
        }


def verify.enforce_max_cycles [Monad.MonadVerify M Elem ExtElem] [Algebraic Elem ExtElem]: M Unit
  := do let po2 <- MonadVerifyAdapter.get_po2
        if po2 > Constants.MAX_CYCLES_PO2 then throw (VerificationError.TooManyCycles po2 Constants.MAX_CYCLES_PO2)
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
        -- Next step: FRI verify!
        throw (VerificationError.Sorry "Zkvm.Verify.verify")


def run_verify [Algebraic Elem ExtElem] (circuit: Circuit Elem ExtElem) (method_id: MethodId) (journal seal: Array UInt32)
  := Monad.VerifyContext.run circuit method_id seal (verify journal)

end Zkvm.Verify
