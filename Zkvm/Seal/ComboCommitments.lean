/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.ArithVM.Circuit
import Zkvm.ArithVM.Taps
import Zkvm.Seal.CheckCommitments
import Zkvm.Seal.Header
import Zkvm.Seal.TraceCommitments
import Zkvm.Verify.Classes
import Zkvm.Verify.Monad

namespace Zkvm.Seal.ComboCommitments

open R0sy.Algebra
open Zkvm.ArithVM.Circuit
open Zkvm.ArithVM.Taps
open Zkvm.Verify.Classes
open Zkvm.Verify.Monad

structure ComboCommitments (ExtElem: Type) where
  mix: ExtElem
  combo_u: Array ExtElem

def read_and_commit (Elem ExtElem: Type) [MonadVerify M] [Algebraic Elem ExtElem] (check_commitments: CheckCommitments.CheckCommitments ExtElem): M (ComboCommitments ExtElem)
  := do let circuit <- MonadCircuit.getCircuit
        let mix: ExtElem <- Field.random
        let mut combo_u: Array ExtElem := Array.mkArray (circuit.taps.tot_combo_backs.toNat + 1) Ring.zero
        -- TODO: reuse Circuit.tap_cache instead of computing tap_mix_pows and check_mix_pows
        let mut cur_mix: ExtElem := Ring.one
        let mut cur_pos := 0
        -- Handle the tap group
        for reg in (TapSet.regIter circuit.taps) do
          let reg_size := RegRef.size reg
          for i in [0:reg_size] do
            let idx := circuit.taps.combo_begin[reg.combo_id]!.toNat + i
            let val := combo_u[idx]! + cur_mix * check_commitments.coeff_u[cur_pos + i]!
            combo_u := Array.set! combo_u idx val
          cur_mix := cur_mix * mix
          cur_pos := cur_pos + reg_size
        -- Handle check group
        for _ in [0:Circuit.check_size Elem ExtElem] do
          let idx := circuit.taps.tot_combo_backs.toNat
          let val := combo_u[idx]! + cur_mix * check_commitments.coeff_u[cur_pos]!
          combo_u := Array.set! combo_u idx val
          cur_pos := cur_pos + 1
          cur_mix := cur_mix * mix
        pure {
          mix,
          combo_u
        }

end Zkvm.Seal.ComboCommitments
