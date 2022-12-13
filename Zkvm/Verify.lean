/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.ArithVM.Circuit
import Zkvm.Constants
import Zkvm.MethodId
import Zkvm.ArithVM.Taps
import Zkvm.Seal.Header
import Zkvm.Verify.Classes
import Zkvm.Verify.Fri
import Zkvm.Verify.Merkle
import Zkvm.Verify.Monad

namespace Zkvm.Verify

open R0sy.Algebra
open R0sy.Algebra.Poly
open R0sy.Hash.Sha2
open R0sy.Lean.Nat
open R0sy.Serial
open ArithVM.Circuit
open ArithVM.Taps
open Classes
open Fri
open Merkle
open MethodId
open Seal

structure MerkleVerifiers where
  code_merkle: MerkleTreeVerifier
  data_merkle: MerkleTreeVerifier
  accum_merkle: MerkleTreeVerifier

def MerkleVerifiers.check_code_root [Monad.MonadVerify M Elem ExtElem] [Algebraic Elem ExtElem] (header: Header.Header Elem) (code_merkle: MerkleTreeVerifier): M Unit
  := do let method_id <- MonadMethodId.getMethodId
        let which := header.po2 - Constants.MIN_CYCLES_PO2
        if which >= method_id.table.size then throw (VerificationError.MethodCycleError header.po2)
        if method_id.table[which]! != MerkleTreeVerifier.root code_merkle then throw VerificationError.MethodVerificationError
        pure ()

def MerkleVerifiers.new [Monad.MonadVerify M Elem ExtElem] [Algebraic Elem ExtElem] (header: Header.Header Elem): M MerkleVerifiers
  := do let circuit <- MonadCircuit.getCircuit
        let code_merkle <- MerkleTreeVerifier.new header.domain (TapSet.groupSize circuit.taps RegisterGroup.Code) Constants.QUERIES
        MerkleVerifiers.check_code_root header code_merkle
        let data_merkle <- MerkleTreeVerifier.new header.domain (TapSet.groupSize circuit.taps RegisterGroup.Data) Constants.QUERIES
        MonadVerifyAdapter.accumulate
        let accum_merkle <- MerkleTreeVerifier.new header.domain (TapSet.groupSize circuit.taps RegisterGroup.Accum) Constants.QUERIES
        pure {
          code_merkle,
          data_merkle,
          accum_merkle
        }


structure CheckVerifier (ExtElem: Type) where
  check_merkle: MerkleTreeVerifier
  z: ExtElem
  mix: ExtElem
  combo_u: Array ExtElem

def CheckVerifier.evaluate [Monad.MonadVerify M Elem ExtElem] [Algebraic Elem ExtElem] (header: Header.Header Elem) (regs: RegIter) (z: ExtElem) (coeff_u: Array ExtElem): M (Array ExtElem)
  := do let circuit <- MonadCircuit.getCircuit
        let mut cur_pos := 0
        let mut eval_u: Array ExtElem := Array.mkEmpty (TapSet.tapSize circuit.taps)
        for reg in regs do
          let reg_size := RegRef.size reg
          for i in [0:reg_size] do
            let x := z * Algebra.ofBase (header.back_one ^ (RegRef.back reg i))
            let poly: Poly ExtElem := Poly.ofSubarray (coeff_u.toSubarray cur_pos (cur_pos + reg_size))
            let fx := Poly.eval poly x
            eval_u := eval_u.push fx
          cur_pos := cur_pos + reg_size
        pure eval_u

def CheckVerifier.new [Monad.MonadVerify M Elem ExtElem] [Algebraic Elem ExtElem] (header: Header.Header Elem): M (CheckVerifier ExtElem)
  := do let circuit <- MonadCircuit.getCircuit
        let poly_mix: ExtElem <- Field.random
        let check_merkle <- MerkleTreeVerifier.new header.domain circuit.check_size Constants.QUERIES
        let z: ExtElem <- Field.random
        -- Read the U coeffs + commit their hash
        let circuit <- MonadCircuit.getCircuit
        let num_taps := TapSet.tapSize circuit.taps
        let coeff_u <- MonadReadIop.readFields ExtElem (num_taps + circuit.check_size)
        let hash_u := Sha256.hash_pod coeff_u
        MonadReadIop.commit hash_u
        -- Now convert to evaluated values
        let eval_u <- CheckVerifier.evaluate header (TapSet.regIter circuit.taps) z coeff_u
        -- Compute the core polynomial
        let mix <- MonadVerifyAdapter.get_mix
        let result := (circuit.poly_ext poly_mix eval_u #[header.journal, mix]).tot
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
        check := check * ((Ring.ofNat 3 * z) ^ header.size - Ring.one)
        if check != result then throw (VerificationError.InvalidCheck (ToString.toString result) (ToString.toString check))
        -- Set the mix value
        let mix: ExtElem <- Field.random
        -- Construct combo_u
        let mut combo_u: Array ExtElem := Array.mkArray (circuit.taps.tot_combo_backs.toNat + 1) Ring.zero
        -- TODO: reuse Circuit.tap_cache instead of computing tap_mix_pows and check_mix_pows
        let mut cur_mix: ExtElem := Ring.one
        let mut cur_pos := 0
        -- Handle the tap group
        for reg in (TapSet.regIter circuit.taps) do
          let reg_size := RegRef.size reg
          for i in [0:reg_size] do
            let idx := circuit.taps.combo_begin[reg.combo_id]!.toNat + i
            let val := combo_u[idx]! + cur_mix * coeff_u[cur_pos + i]!
            combo_u := Array.set! combo_u idx val
          cur_mix := cur_mix * mix
          cur_pos := cur_pos + reg_size
        -- Handle check group
        for _ in [0:circuit.check_size] do
          let idx := circuit.taps.tot_combo_backs.toNat
          let val := combo_u[idx]! + cur_mix * coeff_u[cur_pos]!
          combo_u := Array.set! combo_u idx val
          cur_pos := cur_pos + 1
          cur_mix := cur_mix * mix
        pure {
          check_merkle,
          z,
          mix,
          combo_u
        }

def verify.fri_eval_taps [Monad M] [MonadExceptOf VerificationError M] [Algebraic Elem ExtElem]
  (circuit: Circuit Elem ExtElem) (check_verifier: CheckVerifier ExtElem) (check_row: Array Elem) (back_one: Elem) (x z: ExtElem) (rows: Array (Array Elem)): M ExtElem
  := do let combos_count := circuit.taps.combos_count.toNat
        let mut tot: Array ExtElem := Array.mkArray (combos_count + 1) Ring.zero
        let mut cur_mix: ExtElem := Ring.one
        -- Tap group
        for reg in circuit.taps.regIter do
          let idx := reg.combo_id
          tot := Array.setD tot idx (tot[idx]! + cur_mix * rows[reg.group.toNat]![reg.offset]!)
          cur_mix := cur_mix * check_verifier.mix
        -- Check group
        for i in [0:circuit.check_size] do
          tot := Array.setD tot combos_count (tot[combos_count]! + cur_mix * check_row[i]!)
          cur_mix := cur_mix * check_verifier.mix
        -- Compute the return value
        let mut ret: ExtElem := Ring.zero
        for i in [0:combos_count] do
          let start := circuit.taps.combo_begin[i]!.toNat
          let stop := circuit.taps.combo_begin[i + 1]!.toNat
          let poly: Poly ExtElem := Poly.ofSubarray (check_verifier.combo_u.toSubarray start stop)
          let num := tot[i]! - Poly.eval poly x
          let mut divisor: ExtElem := Ring.one
          for back in (circuit.taps.getCombo i).slice do
            divisor := divisor * (x - z * back_one ^ back.toNat)
          ret := ret + num / divisor
        let check_num := tot[combos_count]! - check_verifier.combo_u[circuit.taps.tot_combo_backs.toNat]!
        let check_div := x - z ^ Constants.INV_RATE
        ret := ret + check_num / check_div
        pure ret

def verify.fri [Monad.MonadVerify M Elem ExtElem] [Algebraic Elem ExtElem] (header: Header.Header Elem) (merkle_verifiers: MerkleVerifiers) (check_verifier: CheckVerifier ExtElem): M Unit
  := do let circuit <- MonadCircuit.getCircuit
        let gen: Elem := RootsOfUnity.ROU_FWD[Nat.log2_ceil (header.domain)]!
        fri_verify Elem ExtElem header.size (fun idx
          => do let x := gen ^ idx
                let rows: Array (Array Elem) := #[
                  <- merkle_verifiers.accum_merkle.verify idx,
                  <- merkle_verifiers.code_merkle.verify idx,
                  <- merkle_verifiers.data_merkle.verify idx
                ]
                let check_row: Array Elem <- check_verifier.check_merkle.verify idx
                verify.fri_eval_taps
                  circuit
                  check_verifier
                  check_row
                  header.back_one
                  (Algebra.ofBase x)
                  check_verifier.z
                  rows
        )

def verify.enforce_max_cycles [Monad.MonadVerify M Elem ExtElem] [Algebraic Elem ExtElem] (header: Header.Header Elem): M Unit
  := if header.po2 > Constants.MAX_CYCLES_PO2
      then throw (VerificationError.TooManyCycles header.po2 Constants.MAX_CYCLES_PO2)
      else pure ()

def verify (journal: Array UInt32) [Monad.MonadVerify M Elem ExtElem] [Algebraic Elem ExtElem]: M Unit
  := do -- Read the header and verify the journal
        let header <- MonadCircuit.getCircuit >>= Header.read
        Header.verify_journal header journal
        -- Enforce constraints on cycle count
        verify.enforce_max_cycles header
        -- Get the Merkle trees
        let merkle_verifiers <- MerkleVerifiers.new header
        -- Begin the check process
        let check_verifier <- CheckVerifier.new header
        -- FRI verify
        verify.fri header merkle_verifiers check_verifier
        -- Ensure proof buffer is empty
        MonadReadIop.verifyComplete

def run_verify [Algebraic Elem ExtElem] (circuit: Circuit Elem ExtElem) (method_id: MethodId) (journal seal: Array UInt32): Except VerificationError Unit
  := Monad.VerifyContext.run circuit method_id seal (verify journal)

end Zkvm.Verify
