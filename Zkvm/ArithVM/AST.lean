/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.ArithVM.Taps

namespace Zkvm.ArithVM.AST

open R0sy.Algebra

structure MixState (ExtElem: Type) where
  tot: ExtElem
  mul: ExtElem
  deriving Inhabited

structure Arg where
  rep: UInt32

structure Var where
  rep: UInt32

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

def PolyExtStepDef.run [Field Elem] [Field ExtElem] [Algebra Elem ExtElem] (self: PolyExtStepDef) (mix: ExtElem) (u: Array ExtElem) (args: Array (Array Elem)): MixState ExtElem :=
  Id.run do let mut fp_vars: Array ExtElem := Array.mkEmpty (self.block.size - (self.ret.rep.toNat + 1))
            let mut mix_vars: Array (MixState ExtElem) := Array.mkEmpty (self.ret.rep.toNat + 1)
            for op in self.block do
              (fp_vars, mix_vars) := PolyExtStep.step op fp_vars mix_vars mix u args
            pure mix_vars[self.ret.rep.toNat]!

end Zkvm.ArithVM.AST
