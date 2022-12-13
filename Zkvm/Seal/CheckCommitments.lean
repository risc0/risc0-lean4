/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.ArithVM.Circuit
import Zkvm.ArithVM.Taps
import Zkvm.Seal.Header
import Zkvm.Seal.TraceCommitments
import Zkvm.Verify.Classes
import Zkvm.Verify.Merkle
import Zkvm.Verify.Monad

namespace Zkvm.Seal.CheckCommitments

open R0sy.Algebra
open R0sy.Algebra.Poly
open R0sy.Hash.Sha2
open Zkvm.ArithVM.Circuit
open Zkvm.ArithVM.Taps
open Zkvm.Verify.Classes
open Zkvm.Verify.Merkle
open Zkvm.Verify.Monad

structure CheckVerifier (ExtElem: Type) where
  check_merkle: MerkleTreeVerifier
  z: ExtElem
  mix: ExtElem
  combo_u: Array ExtElem

def evaluate [MonadVerify M] [Algebraic Elem ExtElem] (header: Header.Header Elem) (regs: RegIter) (z: ExtElem) (coeff_u: Array ExtElem): M (Array ExtElem)
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

def read_and_commit [MonadVerify M] [Algebraic Elem ExtElem] (header: Header.Header Elem) (trace_commitments: TraceCommitments.TraceVerifier Elem): M (CheckVerifier ExtElem)
  := do let poly_mix: ExtElem <- Field.random
        let check_merkle <- MerkleTreeVerifier.read_and_commit header.domain (Circuit.check_size Elem ExtElem) Constants.QUERIES
        let z: ExtElem <- Field.random
        -- Read the U coeffs + commit their hash
        let circuit <- MonadCircuit.getCircuit
        let num_taps := TapSet.tapSize circuit.taps
        let coeff_u <- MonadReadIop.readFields ExtElem (num_taps + Circuit.check_size Elem ExtElem)
        let hash_u := Sha256.hash_pod coeff_u
        MonadReadIop.commit hash_u
        -- Now convert to evaluated values
        let eval_u <- evaluate header (TapSet.regIter circuit.taps) z coeff_u
        -- Compute the core polynomial
        let result := (circuit.poly_ext poly_mix eval_u #[header.output, trace_commitments.mix]).tot
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
        for _ in [0:Circuit.check_size Elem ExtElem] do
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

end Zkvm.Seal.CheckCommitments
