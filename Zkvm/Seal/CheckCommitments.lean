/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.ArithVM.Circuit
import Zkvm.ArithVM.Taps
import Zkvm.Seal.Header
import Zkvm.Seal.TraceCommitments
import Zkvm.Verify.Error
import Zkvm.Verify.Merkle
import Zkvm.Verify.ReadIop

namespace Zkvm.Seal.CheckCommitments

open R0sy.Algebra
open R0sy.Algebra.Poly
open R0sy.Hash
open R0sy.Hash.Sha2
open Zkvm.ArithVM.Circuit
open Zkvm.ArithVM.Taps
open Zkvm.Verify.Error
open Zkvm.Verify.Merkle
open Zkvm.Verify.ReadIop


def compute_u
    (circuit: Circuit)
    (header: Header.Header circuit.field.Elem)
    (z: circuit.field.ExtElem)
    (coeff_u: Array circuit.field.ExtElem)
    (mix: circuit.field.ExtElem)
    (args: Array (Array circuit.field.Elem))
    : circuit.field.ExtElem
  := Id.run do
        let mut cur_pos := 0
        let mut eval_u := Array.mkEmpty (TapSet.tapSize circuit.taps)
        for reg in TapSet.regIter circuit.taps do
          let reg_size := RegRef.size reg
          for i in [0:reg_size] do
            let x := z * Algebra.ofBase (header.back_one ^ (RegRef.back reg i))
            let poly: Poly circuit.field.ExtElem := Poly.ofSubarray (coeff_u.toSubarray cur_pos (cur_pos + reg_size))
            let fx := Poly.eval poly x
            eval_u := eval_u.push fx
          cur_pos := cur_pos + reg_size
        pure (circuit.poly_ext mix eval_u args).tot

def compute_check_u
    [Algebraic Elem ExtElem]
    (header: Header.Header Elem)
    (num_taps: Nat)
    (z: ExtElem)
    (coeff_u: Array ExtElem)
    : ExtElem
  := Id.run do
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
        pure (check * ((Ring.ofNat 3 * z) ^ header.size - Ring.one))


structure CheckCommitments (ExtElem: Type) where
  check_merkle: MerkleTreeVerifier
  z: ExtElem
  coeff_u: Array ExtElem

def read_and_commit
    [Monad M]
    [MonadReadIop M]
    [MonadExceptOf VerificationError M]
    (circuit: Circuit)
    (header: Header.Header circuit.field.Elem)
    (trace_commitments_mix: Array circuit.field.Elem)
    : M (CheckCommitments circuit.field.ExtElem)
  := do let poly_mix: circuit.field.ExtElem <- Field.random
        let check_merkle <- MerkleTreeVerifier.read_and_commit header.domain circuit.check_size Constants.QUERIES
        let z: circuit.field.ExtElem <- Field.random
        let num_taps := TapSet.tapSize circuit.taps
        let coeff_u <- MonadReadIop.readFields circuit.field.ExtElem (num_taps + circuit.check_size)
        let result := compute_u circuit header z coeff_u poly_mix #[header.output, trace_commitments_mix]
        let check := compute_check_u header num_taps z coeff_u
        if check != result then throw (VerificationError.InvalidCheck (ToString.toString result) (ToString.toString check))
        MonadReadIop.commit (Sha256.hash_pod coeff_u)
        pure {
          check_merkle,
          z,
          coeff_u,
        }


structure Combos (ExtElem: Type) where
  tap_cache: TapCache ExtElem
  combo_u: Array ExtElem

def compute_combos
    (circuit: Circuit)
    (self: CheckCommitments.CheckCommitments circuit.field.ExtElem)
    (mix: circuit.field.ExtElem)
    : Combos circuit.field.ExtElem
  := Id.run do
        let tap_cache := circuit.tap_cache mix
        let mut combo_u: Array circuit.field.ExtElem := Array.mkArray (circuit.taps.tot_combo_backs.toNat + 1) Ring.zero
        let mut cur_pos := 0
        -- Tap group
        let mut tap_cache_idx := 0
        for reg in TapSet.regIter circuit.taps do
          let reg_size := RegRef.size reg
          for i in [0:reg_size] do
            let idx := circuit.taps.combo_begin[reg.combo_id]!.toNat + i
            let val := combo_u[idx]! + tap_cache.tap_mix_pows[tap_cache_idx]! * self.coeff_u[cur_pos + i]!
            combo_u := Array.set! combo_u idx val
          tap_cache_idx := tap_cache_idx + 1
          cur_pos := cur_pos + reg_size
        -- Check group
        for i in [0:circuit.check_size] do
          let idx := circuit.taps.tot_combo_backs.toNat
          let val := combo_u[idx]! + tap_cache.check_mix_pows[i]! * self.coeff_u[cur_pos]!
          combo_u := Array.set! combo_u idx val
          cur_pos := cur_pos + 1
        pure {
          tap_cache,
          combo_u
        }

end Zkvm.Seal.CheckCommitments