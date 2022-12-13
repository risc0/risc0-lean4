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

@[always_inline]
instance BabyBear.Algebraic : Algebraic BabyBear.Elem BabyBear.ExtElem where
  prime_field := BabyBear.Elem.PrimeField
  prime_rou := BabyBear.Elem.RootsOfUnity
  ext_field := BabyBear.ExtElem.Field
  ext_rou := BabyBear.ExtElem.RootsOfUnity
  alg := BabyBear.ElemExt.Elem.Algebra
  ext := BabyBear.ElemExt.Elem.ExtField

@[always_inline]
instance BabyBear2.Algebraic : Algebraic BabyBear2.Elem BabyBear2.ExtElem where
  prime_field := BabyBear2.Elem.PrimeField
  prime_rou := BabyBear2.Elem.RootsOfUnity
  ext_field := BabyBear2.ExtElem.Field
  ext_rou := BabyBear2.ExtElem.RootsOfUnity
  alg := BabyBear2.ElemExt.Elem.Algebra
  ext := BabyBear2.ElemExt.Elem.ExtField

@[always_inline]
instance Goldilocks.Algebraic : Algebraic Goldilocks.Elem Goldilocks.ExtElem where
  prime_field := Goldilocks.Elem.PrimeField
  prime_rou := Goldilocks.Elem.RootsOfUnity
  ext_field := Goldilocks.ExtElem.Field
  ext_rou := Goldilocks.ExtElem.RootsOfUnity
  alg := Goldilocks.ElemExt.Elem.Algebra
  ext := Goldilocks.ElemExt.Elem.ExtField


structure Circuit where
  output_size: Nat
  mix_size: Nat
  taps: TapSet
  polydef: PolyExtStepDef

def Circuit.byteRead [Monad M] [MonadByteReader M]: M Circuit
  := do let output_size <- MonadByteReader.readUInt32le >>= (fun x => pure <| x.toNat)
        let mix_size <- MonadByteReader.readUInt32le >>= (fun x => pure <| x.toNat)
        let taps <- TapSet.byteRead
        let polydef <- PolyExtStepDef.byteRead
        pure {
          output_size,
          mix_size,
          taps,
          polydef
        }

def Circuit.ofFile (filename: System.FilePath): IO Circuit
  := do let meta <- filename.metadata
        let byteSize := meta.byteSize
        let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
        let bytes <- handle.read (byteSize.toNat.toUSize)
        let result := R0sy.ByteDeserial.ByteReader.run Circuit.byteRead bytes.data.toSubarray
        match result with
        | Except.ok circuit => pure circuit
        | Except.error error => panic! s!"ERROR: {error}"

def Circuit.check_size (Elem ExtElem: Type) [ExtField Elem ExtElem] := Constants.INV_RATE * (ExtField.EXT_DEG Elem ExtElem)

def Circuit.poly_ext [Field Elem] [Field ExtElem] [Algebra Elem ExtElem] (self: Circuit) (mix: ExtElem) (u: Array ExtElem) (args: Array (Array Elem)): MixState ExtElem
  := PolyExtStepDef.run self.polydef mix u args

structure TapCache (ExtElem: Type) where
  tap_mix_pows: Array ExtElem
  check_mix_pows: Array ExtElem

def Circuit.tap_cache [Field Elem] [Field ExtElem] [ExtField Elem ExtElem] (self: Circuit) (mix: ExtElem): TapCache ExtElem
  := Id.run do
      let mut cur_mix: ExtElem := Ring.one
      let mut tap_mix_pows := Array.mkEmpty self.taps.reg_count.toNat
      for _ in self.taps.regIter do
        tap_mix_pows := tap_mix_pows.push cur_mix
        cur_mix := cur_mix * mix
      let mut check_mix_pows := Array.mkEmpty (check_size Elem ExtElem)
      for _ in [0:check_size Elem ExtElem] do
        check_mix_pows := check_mix_pows.push cur_mix
        cur_mix := cur_mix * mix
      pure {
        tap_mix_pows,
        check_mix_pows
      }

end Zkvm.ArithVM.Circuit
