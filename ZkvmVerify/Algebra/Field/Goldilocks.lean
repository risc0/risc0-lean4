/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import ZkvmVerify.Algebra
import ZkvmVerify.Algebra.Field
import ZkvmVerify.Algebra.Poly

namespace ZkvmVerify.Algebra.Field.Goldilocks

open Poly

/- Base field -/

def P: Prime := {
  rep := 2^64 - 2^32 + 1,
  pos := sorry
}

structure Elem where
  rep: Prime.Elem P
  deriving Repr

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
  rep := Poly.mono 2 Ring.one - Poly.mono 0 (Ring.ofNat 11)
}

structure ExtElem where
  rep: Ext.Elem Q
  deriving Repr

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

end ZkvmVerify.Algebra.Field.Goldilocks
