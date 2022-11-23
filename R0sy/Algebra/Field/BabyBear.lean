/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import R0sy.Algebra.Field
import R0sy.Algebra.Poly

namespace R0sy.Algebra.Field.BabyBear

open Poly

/- Base field -/

def P: Prime := {
  rep := 15 * 2^27 + 1,
  pos := sorry
}

structure Elem where
  rep: Prime.Elem P
  deriving Repr

instance : OfNat Elem n where ofNat := { rep := Prime.Elem.ofNat _ n }

instance : BEq Elem where beq x y := x.rep == y.rep

instance : Add Elem where add x y := { rep := x.rep + y.rep }

instance : Neg Elem where neg x := { rep := - x.rep }

instance : Sub Elem where sub x y := { rep := x.rep - y.rep }

instance : Mul Elem where mul x y := { rep := x.rep * y.rep }

instance : HPow Elem Nat Elem where hPow x n := { rep := x.rep ^ n }

instance : Ring Elem where
  ofNat n := { rep := Ring.ofNat n }

instance : Div Elem where div x y := { rep := x.rep / y.rep }

instance : Field Elem where inv x := { rep := x.rep.inv }


/- Extension field -/

def Q: Irreducible Elem (Poly.Poly Elem) := {
  rep := Poly.mono 4 Ring.one - Poly.mono 0 (Ring.ofNat 11)
}

structure ExtElem where
  rep: Ext.Elem Q
  deriving Repr

instance : OfNat ExtElem n where ofNat := { rep := Ext.Elem.ofNat _ n }

instance : BEq ExtElem where beq x y := x.rep == y.rep

instance : Add ExtElem where add x y := { rep := x.rep + y.rep }

instance : Neg ExtElem where neg x := { rep := - x.rep }

instance : Sub ExtElem where sub x y := { rep := x.rep - y.rep }

instance : Mul ExtElem where mul x y := { rep := x.rep * y.rep }

instance : HPow ExtElem Nat ExtElem where hPow x n := { rep := x.rep ^ n }

instance : Ring ExtElem where
  ofNat n := { rep := Ring.ofNat n }

instance : Div ExtElem where div x y := { rep := x.rep / y.rep }

instance : Field ExtElem where inv x := { rep := x.rep.inv }


/- The extension is an algebra over the base -/

instance : Algebra Elem ExtElem where
  ofBase c := { rep := Algebra.ofBase c }


/- Examples -/

example:
  let base: Nat -> Elem := Ring.ofNat
  base 3 + base 7 == base 10
  := by rfl

example:
  let base: Nat -> Elem := Ring.ofNat
  (- base 3) == base (BabyBear.P.rep - 3)
  := by rfl

example:
  let base: Nat -> Elem := Ring.ofNat
  base 5 - base 8 == - base 3
  := by rfl

example:
  let base: Nat -> Elem := Ring.ofNat
  base 8 * base 5 == base 40
  := by rfl

#eval
  let base: Nat -> Elem := Ring.ofNat
  (base 2) ^ 3 == base 8

#eval
  let base: Nat -> Elem := Ring.ofNat
  (base 19) / (base 19) == base 1

#eval
  let x: ExtElem := { rep := { rep := { rep := #[3432, 213424, 765, 235465] } } }
  x / x == 1

end R0sy.Algebra.Field.BabyBear
