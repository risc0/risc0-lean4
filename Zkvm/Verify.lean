/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.ArithVM.Circuit
import Zkvm.Constants
import Zkvm.MethodId
import Zkvm.Seal.CheckCommitments
import Zkvm.Seal.Fri
import Zkvm.Seal.Header
import Zkvm.Seal.TraceCommitments
import Zkvm.Verify.Error
import Zkvm.Verify.ReadIop

namespace Zkvm.Verify

open R0sy.Algebra
open R0sy.Algebra.Poly
open R0sy.Lean.Nat
open Zkvm.ArithVM.Circuit
open Zkvm.MethodId
open Zkvm.Seal
open Zkvm.Verify.Error
open Zkvm.Verify.ReadIop


def eval_taps [Algebraic Elem ExtElem] (circuit: Circuit) (check_commitments: CheckCommitments.CheckCommitments ExtElem) (combo_commitments: CheckCommitments.Combos ExtElem) (rows: Array (Array Elem)) (check_row: Array Elem) (back_one: Elem) (x: ExtElem): ExtElem
  := Id.run do
        let combos_count := circuit.taps.combos_count.toNat
        let mut tot: Array ExtElem := Array.mkArray (combos_count + 1) Ring.zero
        -- Tap group
        let mut tap_cache_idx := 0
        for reg in circuit.taps.regIter do
          let idx := reg.combo_id
          let val := tot[idx]! + combo_commitments.tap_cache.tap_mix_pows[tap_cache_idx]! * rows[reg.group.toNat]![reg.offset]!
          tot := Array.set! tot idx val
          tap_cache_idx := tap_cache_idx + 1
        -- Check group
        for i in [0:Circuit.check_size Elem ExtElem] do
          tot := Array.setD tot combos_count (tot[combos_count]! + combo_commitments.tap_cache.check_mix_pows[i]! * check_row[i]!)
        -- Compute the return value
        let mut ret: ExtElem := Ring.zero
        for i in [0:combos_count] do
          let start := circuit.taps.combo_begin[i]!.toNat
          let stop := circuit.taps.combo_begin[i + 1]!.toNat
          let poly: Poly ExtElem := Poly.ofSubarray (combo_commitments.combo_u.toSubarray start stop)
          let mut divisor: ExtElem := Ring.one
          for back in (circuit.taps.getCombo i).slice do
            divisor := divisor * (x - check_commitments.z * back_one ^ back.toNat)
          ret := ret + (tot[i]! - Poly.eval poly x) / divisor
        let check_num := tot[combos_count]! - combo_commitments.combo_u[circuit.taps.tot_combo_backs.toNat]!
        let check_div := x - check_commitments.z ^ Constants.INV_RATE
        ret := ret + check_num / check_div
        pure ret


def verify (Elem ExtElem: Type) [Algebraic Elem ExtElem] (circuit: Circuit) (method_id: MethodId) (journal: Array UInt32) {M: Type -> Type} [Monad M] [MonadExceptOf VerificationError M] [MonadReadIop M]: M Unit
  := do -- Read the header and verify the journal
        let header <- Header.read circuit
        Header.verify_journal header journal
        -- Enforce constraints on cycle count
        if header.po2 > Constants.MAX_CYCLES_PO2 then throw (VerificationError.TooManyCycles header.po2 Constants.MAX_CYCLES_PO2)
        -- Read the commitments
        let trace_commitments <- TraceCommitments.read_and_commit circuit method_id header
        let check_commitments <- CheckCommitments.read_and_commit circuit header trace_commitments.mix
        -- FRI verify
        let combo_commitments <- CheckCommitments.compute_combos Elem ExtElem circuit check_commitments
        let fri_verify_params <- Fri.read_and_commit Elem ExtElem header.size
        let gen: Elem := RootsOfUnity.ROU_FWD[Nat.log2_ceil (header.domain)]!
        Fri.verify fri_verify_params (fun idx
          => do let rows: Array (Array Elem) := #[
                  <- trace_commitments.accum_merkle.verify idx,
                  <- trace_commitments.code_merkle.verify idx,
                  <- trace_commitments.data_merkle.verify idx
                ]
                let check_row: Array Elem <- check_commitments.check_merkle.verify idx
                let result := eval_taps
                  circuit
                  check_commitments
                  combo_commitments
                  rows
                  check_row
                  header.back_one
                  (Algebra.ofBase (gen ^ idx))
                pure result
        )
        -- Ensure proof buffer is empty
        MonadReadIop.verifyComplete

end Zkvm.Verify
