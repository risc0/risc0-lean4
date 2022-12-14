/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.ArithVM.AST
import Zkvm.ArithVM.Taps
import Zkvm.Constants

namespace Zkvm.ArithVM.Circuit

open R0sy.Algebra
open R0sy.Algebra.Field
open R0sy.Algebra.Poly
open R0sy.ByteDeserial
open AST
open Taps


class Algebraic (Elem ExtElem: Type) where
  prime_field: PrimeField Elem
  prime_rou: RootsOfUnity Elem
  ext_field: Field ExtElem
  ext_rou: RootsOfUnity ExtElem
  alg: Algebra Elem ExtElem
  ext: ExtField Elem ExtElem

@[always_inline]
instance [Algebraic Elem ExtElem] : PrimeField Elem := Algebraic.prime_field ExtElem

@[always_inline]
instance [Algebraic Elem ExtElem] : RootsOfUnity Elem := Algebraic.prime_rou ExtElem

@[always_inline]
instance [Algebraic Elem ExtElem] : Field ExtElem := Algebraic.ext_field Elem

@[always_inline]
instance [Algebraic Elem ExtElem] : RootsOfUnity ExtElem := Algebraic.ext_rou Elem

@[always_inline]
instance [Algebraic Elem ExtElem] : Algebra Elem ExtElem := Algebraic.alg

@[always_inline]
instance [Algebraic Elem ExtElem] : ExtField Elem ExtElem := Algebraic.ext


inductive CircuitField where
  | BabyBear
  | BabyBear2
  | Goldilocks

@[always_inline, inline]
def CircuitField.Elem: CircuitField -> Type
  | .BabyBear => BabyBear.Elem
  | .BabyBear2 => BabyBear2.Elem
  | .Goldilocks => Goldilocks.Elem

@[always_inline, inline]
def CircuitField.ExtElem: CircuitField -> Type
  | .BabyBear => BabyBear.ExtElem
  | .BabyBear2 => BabyBear2.ExtElem
  | .Goldilocks => Goldilocks.ExtElem

@[always_inline]
instance : Algebraic (CircuitField.Elem f) (CircuitField.ExtElem f) where
  prime_field :=
    match f with
    | .BabyBear => BabyBear.Elem.PrimeField
    | .BabyBear2 => BabyBear2.Elem.PrimeField
    | .Goldilocks => Goldilocks.Elem.PrimeField
  prime_rou :=
    match f with
    | .BabyBear => BabyBear.Elem.RootsOfUnity
    | .BabyBear2 => BabyBear2.Elem.RootsOfUnity
    | .Goldilocks => Goldilocks.Elem.RootsOfUnity
  ext_field :=
    match f with
    | .BabyBear => BabyBear.ExtElem.Field
    | .BabyBear2 => BabyBear2.ExtElem.Field
    | .Goldilocks => Goldilocks.ExtElem.Field
  ext_rou :=
    match f with
    | .BabyBear => BabyBear.ExtElem.RootsOfUnity
    | .BabyBear2 => BabyBear2.ExtElem.RootsOfUnity
    | .Goldilocks => Goldilocks.ExtElem.RootsOfUnity
  alg :=
    match f with
    | .BabyBear => BabyBear.ExtElem.Elem.Algebra
    | .BabyBear2 => BabyBear2.ExtElem.Elem.Algebra
    | .Goldilocks => Goldilocks.ExtElem.Elem.Algebra
  ext :=
    match f with
    | .BabyBear => BabyBear.ExtElem.Elem.ExtField
    | .BabyBear2 => BabyBear2.ExtElem.Elem.ExtField
    | .Goldilocks => Goldilocks.ExtElem.Elem.ExtField


structure Circuit where
  field: CircuitField
  output_size: Nat
  mix_size: Nat
  taps: TapSet
  polydef: PolyExtStepDef

def Circuit.byteRead [Monad M] [MonadByteReader M] (field: CircuitField): M Circuit
  := do let output_size <- MonadByteReader.readUInt32le >>= (fun x => pure <| x.toNat)
        let mix_size <- MonadByteReader.readUInt32le >>= (fun x => pure <| x.toNat)
        let taps <- TapSet.byteRead
        let polydef <- PolyExtStepDef.byteRead
        pure {
          field,
          output_size,
          mix_size,
          taps,
          polydef
        }

def Circuit.ofFile (field: CircuitField) (filename: System.FilePath): IO Circuit
  := do let meta <- filename.metadata
        let byteSize := meta.byteSize
        let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
        let bytes <- handle.read (byteSize.toNat.toUSize)
        let result := R0sy.ByteDeserial.ByteReader.run (Circuit.byteRead field) bytes.data.toSubarray
        match result with
        | Except.ok circuit => pure circuit
        | Except.error error => panic! s!"ERROR: {error}"

def Circuit.check_size (self: Circuit): Nat := Constants.INV_RATE * (ExtField.EXT_DEG self.field.Elem self.field.ExtElem)

def Circuit.poly_ext (self: Circuit) (mix: self.field.ExtElem) (u: Array self.field.ExtElem) (args: Array (Array self.field.Elem)): MixState self.field.ExtElem
  := PolyExtStepDef.run self.polydef mix u args

structure TapCache (ExtElem: Type) where
  taps: TapSet
  check_size: Nat
  tap_mix_pows: Array ExtElem
  check_mix_pows: Array ExtElem

def Circuit.tap_cache (self: Circuit) (mix: self.field.ExtElem): TapCache self.field.ExtElem
  := Id.run do
      let mut cur_mix := Ring.one
      let mut tap_mix_pows := Array.mkEmpty self.taps.reg_count.toNat
      for _ in self.taps.regIter do
        tap_mix_pows := tap_mix_pows.push cur_mix
        cur_mix := cur_mix * mix
      let mut check_mix_pows := Array.mkEmpty self.check_size
      for _ in [0:self.check_size] do
        check_mix_pows := check_mix_pows.push cur_mix
        cur_mix := cur_mix * mix
      pure {
        taps := self.taps,
        check_size := self.check_size,
        tap_mix_pows,
        check_mix_pows
      }

def TapCache.eval_taps
    [Algebraic Elem ExtElem]
    (tap_cache: TapCache ExtElem)
    (combo_u: Array ExtElem)
    (back_one: Elem)
    (z: ExtElem)
    (rows: Array (Array Elem))
    (check_row: Array Elem)
    (x: ExtElem)
    : ExtElem
  := Id.run do
        let combos_count := tap_cache.taps.combos_count.toNat
        let mut tot := Array.mkArray (combos_count + 1) Ring.zero
        -- Tap group
        let mut tap_cache_idx := 0
        for reg in tap_cache.taps.regIter do
          let idx := reg.combo_id
          let val := tot[idx]! + tap_cache.tap_mix_pows[tap_cache_idx]! * rows[reg.group.toNat]![reg.offset]!
          tot := Array.set! tot idx val
          tap_cache_idx := tap_cache_idx + 1
        -- Check group
        for i in [0:tap_cache.check_size] do
          tot := Array.setD tot combos_count (tot[combos_count]! + tap_cache.check_mix_pows[i]! * check_row[i]!)
        -- Compute the return value
        let mut ret := Ring.zero
        for i in [0:combos_count] do
          let start := tap_cache.taps.combo_begin[i]!.toNat
          let stop := tap_cache.taps.combo_begin[i + 1]!.toNat
          let poly: Poly ExtElem := Poly.ofSubarray (combo_u.toSubarray start stop)
          let mut divisor := Ring.one
          for back in (tap_cache.taps.getCombo i).slice do
            divisor := divisor * (x - z * back_one ^ back.toNat)
          ret := ret + (tot[i]! - Poly.eval poly x) / divisor
        let check_num := tot[combos_count]! - combo_u[tap_cache.taps.tot_combo_backs.toNat]!
        let check_div := x - z ^ Constants.INV_RATE
        ret := ret + check_num / check_div
        pure ret

end Zkvm.ArithVM.Circuit
