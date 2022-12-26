/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.Algebra.Classes
import Zkvm.ArithVM.Taps

namespace Zkvm.ArithVM.AST

open R0sy.ByteDeserial
open Zkvm.Algebra.Classes

structure MixState (ExtElem: Type) where
  tot: ExtElem
  mul: ExtElem
  deriving Inhabited

structure Arg where
  rep: UInt32

def Arg.byteRead [Monad M] [MonadByteReader M]: M Arg
  := do let rep <- MonadByteReader.readUInt32le
        pure { rep }

structure Var where
  rep: UInt32

def Var.byteRead [Monad M] [MonadByteReader M]: M Var
  := do let rep <- MonadByteReader.readUInt32le
        pure { rep }

inductive PolyExtStep where
  | Const: UInt32 -> PolyExtStep
  | Get: UInt32 -> PolyExtStep
  | GetGlobal: Arg -> UInt32 -> PolyExtStep
  | Add: Var -> Var -> PolyExtStep
  | Sub: Var -> Var -> PolyExtStep
  | Mul: Var -> Var -> PolyExtStep
  | True: PolyExtStep
  | AndEqz: Var -> Var -> PolyExtStep
  | AndCond: Var -> Var -> Var -> PolyExtStep

def PolyExtStep.byteRead [Monad M] [MonadByteReader M]: M PolyExtStep
  := do let opcode <- MonadByteReader.readUInt32le
        match opcode with
        | 1 => do let val <- MonadByteReader.readUInt32le
                  pure (PolyExtStep.Const val)
        | 2 => do let val <- MonadByteReader.readUInt32le
                  pure (PolyExtStep.Get val)
        | 3 => do let base <- Arg.byteRead
                  let offset <- MonadByteReader.readUInt32le
                  pure (PolyExtStep.GetGlobal base offset)
        | 4 => do let x1 <- Var.byteRead
                  let x2 <- Var.byteRead
                  pure (PolyExtStep.Add x1 x2)
        | 5 => do let x1 <- Var.byteRead
                  let x2 <- Var.byteRead
                  pure (PolyExtStep.Sub x1 x2)
        | 6 => do let x1 <- Var.byteRead
                  let x2 <- Var.byteRead
                  pure (PolyExtStep.Mul x1 x2)
        | 7 => pure PolyExtStep.True
        | 8 => do let x <- Var.byteRead
                  let val <- Var.byteRead
                  pure (PolyExtStep.AndEqz x val)
        | 9 => do let x <- Var.byteRead
                  let cond <- Var.byteRead
                  let inner <- Var.byteRead
                  pure (PolyExtStep.AndCond x cond inner)
        | _ => throw ByteReaderError.InvalidData

def PolyExtStep.step [Field Elem] [Field ExtElem] [Algebra Elem ExtElem] (self: PolyExtStep) (fp_vars: Array ExtElem) (mix_vars: Array (MixState ExtElem)) (mix: ExtElem) (u: Array ExtElem) (args: Array (Array Elem)): Array ExtElem × Array (MixState ExtElem)
  := match self with
      | Const val =>
          let elem: Elem := Ring.ofNat val.toNat
          let fp_vars' := fp_vars.push (Algebra.ofBase elem)
          (fp_vars', mix_vars)
      | Get tap => (fp_vars.push u[tap.toNat]!, mix_vars)
      | GetGlobal base offset =>
          let fp_vars' := fp_vars.push (Algebra.ofBase args[base.rep.toNat]![offset.toNat]!)
          (fp_vars', mix_vars)
      | Add x1 x2 =>
          let fp_vars' := fp_vars.push (fp_vars[x1.rep.toNat]! + fp_vars[x2.rep.toNat]!)
          (fp_vars', mix_vars)
      | Sub x1 x2 =>
          let fp_vars' := fp_vars.push (fp_vars[x1.rep.toNat]! - fp_vars[x2.rep.toNat]!)
          (fp_vars', mix_vars)
      | Mul x1 x2 =>
          let fp_vars' := fp_vars.push (fp_vars[x1.rep.toNat]! * fp_vars[x2.rep.toNat]!)
          (fp_vars', mix_vars)
      | True =>
          let mix_vars' := mix_vars.push {
            tot := Ring.zero
            mul := Ring.one
          }
          (fp_vars, mix_vars')
      | AndEqz x val =>
          let x := mix_vars[x.rep.toNat]!
          let val := fp_vars[val.rep.toNat]!
          let mix_vars' := mix_vars.push {
            tot := x.tot + x.mul * val
            mul := x.mul * mix
          }
          (fp_vars, mix_vars')
      | AndCond x cond inner =>
          let x := mix_vars[x.rep.toNat]!
          let cond := fp_vars[cond.rep.toNat]!
          let inner := mix_vars[inner.rep.toNat]!
          let mix_vars' := mix_vars.push {
            tot := x.tot + cond * inner.tot * x.mul
            mul := x.mul * inner.mul
          }
          (fp_vars, mix_vars')


structure PolyExtStepDef where
  block: Array PolyExtStep
  ret: Var

def PolyExtStepDef.byteRead [Monad M] [MonadByteReader M]: M PolyExtStepDef
  := do let block <- MonadByteReader.readArray PolyExtStep.byteRead
        let ret <- Var.byteRead
        pure { block, ret }

def PolyExtStepDef.run [Field Elem] [Field ExtElem] [Algebra Elem ExtElem] (self: PolyExtStepDef) (mix: ExtElem) (u: Array ExtElem) (args: Array (Array Elem)): MixState ExtElem :=
  Id.run do let mut fp_vars: Array ExtElem := Array.mkEmpty (self.block.size - (self.ret.rep.toNat + 1))
            let mut mix_vars: Array (MixState ExtElem) := Array.mkEmpty (self.ret.rep.toNat + 1)
            for op in self.block do
              (fp_vars, mix_vars) := PolyExtStep.step op fp_vars mix_vars mix u args
            pure mix_vars[self.ret.rep.toNat]!

end Zkvm.ArithVM.AST
