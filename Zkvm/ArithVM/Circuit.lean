/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.ArithVM.AST
import Zkvm.ArithVM.Taps

namespace Zkvm.ArithVM.Circuit

open R0sy.Algebra
open R0sy.Algebra.Field
open R0sy.ByteDeserial
open AST
open Taps


class Algebraic (Elem ExtElem: Type) where
  prime: PrimeField Elem
  rou: RootsOfUnity Elem
  ext: Field ExtElem
  alg: Algebra Elem ExtElem

instance [Algebraic Elem ExtElem] : PrimeField Elem := Algebraic.prime ExtElem

instance [Algebraic Elem ExtElem] : RootsOfUnity Elem := Algebraic.rou ExtElem

instance [Algebraic Elem ExtElem] : Field ExtElem := Algebraic.ext Elem

instance [Algebraic Elem ExtElem] : Algebra Elem ExtElem := Algebraic.alg

instance BabyBear.Algebraic : Algebraic BabyBear.Elem BabyBear.ExtElem where
  prime := BabyBear.Elem.PrimeField
  rou := BabyBear.Elem.RootsOfUnity
  ext := BabyBear.ExtElem.Field
  alg := BabyBear.ElemExt.Elem.Algebra

instance Goldilocks.Algebraic : Algebraic Goldilocks.Elem Goldilocks.ExtElem where
  prime := Goldilocks.Elem.PrimeField
  rou := Goldilocks.Elem.RootsOfUnity
  ext := Goldilocks.ExtElem.Field
  alg := Goldilocks.ElemExt.Elem.Algebra


structure Circuit (Elem ExtElem: Type) where
  output_size: Nat
  mix_size: Nat
  taps: TapSet
  polydef: PolyExtStepDef

def Circuit.byteRead (Elem ExtElem: Type) [Monad M] [MonadByteReader M]: M (Circuit Elem ExtElem)
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

def Circuit.ofFile (Elem ExtElem: Type) (filename: System.FilePath): IO (Circuit Elem ExtElem)
  := do let meta <- filename.metadata
        let byteSize := meta.byteSize
        let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
        let bytes <- handle.read (byteSize.toNat.toUSize)
        let result := R0sy.ByteDeserial.ByteReader.run (Circuit.byteRead Elem ExtElem) bytes.data.toSubarray
        match result with
        | Except.ok circuit => pure circuit
        | Except.error error => panic! s!"ERROR: {error}"

def Circuit.poly_ext [Field Elem] [Field ExtElem] [Algebra Elem ExtElem] (self: Circuit Elem ExtElem) (mix: ExtElem) (u: Array ExtElem) (args: Array (Array Elem)): MixState ExtElem
  := PolyExtStepDef.run self.polydef mix u args

def riscv (taps: TapSet): Circuit R0sy.Algebra.Field.BabyBear.Elem R0sy.Algebra.Field.BabyBear.ExtElem where
  output_size := 18
  mix_size := 36
  taps := taps
  polydef := {
    block := #[],       -- TODO!
    ret := { rep := 0 } -- TODO!
  }

end Zkvm.ArithVM.Circuit
