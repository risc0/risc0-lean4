/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import R0sy.Algebra.Field
import R0sy.Algebra.Poly

namespace R0sy.Algebra.Field.Goldilocks

open Poly

/- Base field -/

def PRIME: Nat := 2^64 - 2^32 + 1

def P: Prime := {
  rep := PRIME,
  words := 2,
  random_cutoff := PRIME,
  pos := by simp
}

structure Elem where
  rep: Prime.Elem P
  deriving Repr

instance : Inhabited Elem where default := { rep := Inhabited.default }

instance : OfNat Elem n where ofNat := { rep := Prime.Elem.ofNat _ n }

instance : BEq Elem where beq x y := x.rep == y.rep

instance : Add Elem where add x y := { rep := x.rep + y.rep }

instance : Neg Elem where neg x := { rep := - x.rep }

instance : Sub Elem where sub x y := { rep := x.rep - y.rep }

instance : Mul Elem where mul x y := { rep := x.rep * y.rep }

instance : HPow Elem Nat Elem where hPow x n := { rep := x.rep ^ n }

instance : Ring Elem where ofNat n := { rep := Ring.ofNat n }

instance : Div Elem where div x y := { rep := x.rep / y.rep }

instance : Field Elem where
  inv x := { rep := x.rep.inv }
  words := Field.words (Prime.Elem P)
  random
    := do let rep <- Prime.Elem.random P
          return { rep }
  fromUInt64 x := { rep := Field.fromUInt64 x }
  toUInt32Words x := Field.toUInt32Words x.rep
  fromUInt32Words x := { rep := Field.fromUInt32Words x }


/- Extension field -/

def Q: Irreducible Elem (Poly.Poly Elem) := {
  rep := Poly.mono 2 Ring.one - Poly.mono 0 (Ring.ofNat 11)
}

structure ExtElem where
  rep: Ext.Elem Q
  deriving Repr

instance : Inhabited ExtElem where default := { rep := Inhabited.default }

instance : BEq ExtElem where beq x y := x.rep == y.rep

instance : Add ExtElem where add x y := { rep := x.rep + y.rep }

instance : Neg ExtElem where neg x := { rep := - x.rep }

instance : Sub ExtElem where sub x y := { rep := x.rep - y.rep }

instance : Mul ExtElem where mul x y := { rep := x.rep * y.rep }

instance : HPow ExtElem Nat ExtElem where hPow x n := { rep := x.rep ^ n }

instance : Ring ExtElem where ofNat n := { rep := Ring.ofNat n }

instance : Div ExtElem where div x y := { rep := x.rep / y.rep }

instance : Field ExtElem where
  inv x := { rep := x.rep.inv }
  words := Field.words (Ext.Elem Q)
  random
    := do let rep <- Ext.Elem.random Q
          return { rep }
  fromUInt64 x := { rep := Field.fromUInt64 x }
  toUInt32Words x := Field.toUInt32Words x.rep
  fromUInt32Words x := { rep := Field.fromUInt32Words x }


/- The extension is an algebra over the base -/

instance : Algebra Elem ExtElem where
  ofBase c := { rep := Algebra.ofBase c }


/- Examples -/

#eval
  let base: Nat -> Elem := Ring.ofNat
  let to: Elem -> Array UInt32 := Field.toUInt32Words
  let fr: Subarray UInt32 -> Elem := Field.fromUInt32Words
  base 0xffffffaaaa == fr (to (base 0xffffffaaaa)).toSubarray

#eval
  let base: Nat -> Elem := Ring.ofNat
  let to: Elem -> Array UInt32 := Field.toUInt32Words
  let fr: Subarray UInt32 -> Elem := Field.fromUInt32Words
  base 0xffffffaaaa == fr (to (base 0xffffffaaaa)).toSubarray

#eval
  let x: ExtElem := { rep := { rep := { rep := #[0xffffffaaaa, 0xccccccbbbb] } } }
  let to: ExtElem -> Array UInt32 := Field.toUInt32Words
  let fr: Subarray UInt32 -> ExtElem := Field.fromUInt32Words
  x == fr (to x).toSubarray

end R0sy.Algebra.Field.Goldilocks
