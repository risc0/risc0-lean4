/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import R0sy.Algebra.Field
import R0sy.Algebra.Poly
import R0sy.Serial

namespace R0sy.Algebra.Field.BabyBear

open Poly
open Serial

/- Base field -/

def PRIME: Nat := 15 * 2^27 + 1

def P: Prime := {
  rep := PRIME,
  words := 1,
  random_cutoff := (0xffffffff / PRIME) * PRIME
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

instance : SerialUInt32 Elem where
  words := SerialUInt32.words (Prime.Elem P)
  toUInt32Words x := SerialUInt32.toUInt32Words (Ring.ofNat (2^32) * x.rep)
  fromUInt32Words x := { rep := SerialUInt32.fromUInt32Words x / Ring.ofNat (2^32) }

instance : Field Elem where
  inv x := { rep := x.rep.inv }
  random
    := do let rep <- Prime.Elem.random P
          return { rep }
  fromUInt64 x := { rep := Field.fromUInt64 x }

instance ElemPrimeField : PrimeField Elem where
  toNat x := PrimeField.toNat x.rep

instance ElemRootsOfUnity : RootsOfUnity Elem where
  MAX_ROU_SIZE := 27
  ROU_FWD := #[
    1, 2013265920, 284861408, 1801542727, 567209306, 740045640, 918899846, 1881002012,
    1453957774, 65325759, 1538055801, 515192888, 483885487, 157393079, 1695124103, 2005211659,
    1540072241, 88064245, 1542985445, 1269900459, 1461624142, 825701067, 682402162, 1311873874,
    1164520853, 352275361, 18769, 137
  ]
  ROU_REV := #[
    1, 2013265920, 1728404513, 1592366214, 196396260, 1253260071, 72041623, 1091445674,
    145223211, 1446820157, 1030796471, 2010749425, 1827366325, 1239938613, 246299276,
    596347512, 1893145354, 246074437, 1525739923, 1194341128, 1463599021, 704606912, 95395244,
    15672543, 647517488, 584175179, 137728885, 749463956
  ]


/- Extension field -/

def Q: Irreducible Elem (Poly.Poly Elem) := {
  rep := Poly.mono 4 Ring.one - Poly.mono 0 (Ring.ofNat 11)
}

structure ExtElem where
  rep: Ext.Elem Q
  deriving Repr

def ExtElem.new (a0 a1 a2 a3: Elem): ExtElem := {
  rep := { rep := { rep := #[a0, a1, a2, a3] } }
}

instance : Inhabited ExtElem where default := { rep := Inhabited.default }

instance : OfNat ExtElem n where ofNat := { rep := Ext.Elem.ofNat _ n }

instance : BEq ExtElem where beq x y := x.rep == y.rep

instance : Add ExtElem where add x y := { rep := x.rep + y.rep }

instance : Neg ExtElem where neg x := { rep := - x.rep }

instance : Sub ExtElem where sub x y := { rep := x.rep - y.rep }

instance : Mul ExtElem where mul x y := { rep := x.rep * y.rep }

instance : HPow ExtElem Nat ExtElem where hPow x n := { rep := x.rep ^ n }

instance : Ring ExtElem where ofNat n := { rep := Ring.ofNat n }

instance : Div ExtElem where div x y := { rep := x.rep / y.rep }

instance : SerialUInt32 ExtElem where
  words := SerialUInt32.words (Ext.Elem Q)
  toUInt32Words x := SerialUInt32.toUInt32Words x.rep
  fromUInt32Words x := { rep := SerialUInt32.fromUInt32Words x }

instance ExtElemField : Field ExtElem where
  inv x := { rep := x.rep.inv }
  random
    := do let rep <- Ext.Elem.random Q
          return { rep }
  fromUInt64 x := { rep := Field.fromUInt64 x }


/- The extension is an algebra over the base -/

instance ElemExtElemAlgebra : Algebra Elem ExtElem where
  ofBase c := { rep := Algebra.ofBase c }


/- Examples -/

example:
  let base: Nat -> Elem := Ring.ofNat
  base 3 + base 7 == base 10
  := by rfl

#eval
  let base: Nat -> Elem := Ring.ofNat
  let to: Elem -> Array UInt32 := SerialUInt32.toUInt32Words
  let fr: Subarray UInt32 -> Elem := SerialUInt32.fromUInt32Words
  base 3 == fr (to (base 3)).toSubarray

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

#eval
  let x: ExtElem := { rep := { rep := { rep := #[3432, 213424, 765, 235465] } } }
  let to: ExtElem -> Array UInt32 := SerialUInt32.toUInt32Words
  let fr: Subarray UInt32 -> ExtElem := SerialUInt32.fromUInt32Words
  x == fr (to x).toSubarray

#eval
  let x: Elem := RootsOfUnity.ROU_FWD[15]!
  x ^ (2 ^ 15)

end R0sy.Algebra.Field.BabyBear
