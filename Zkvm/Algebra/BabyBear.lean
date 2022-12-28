/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.Algebra.Classes

namespace Zkvm.Algebra.BabyBear

open R0sy.Hash
open R0sy.Serial
open Zkvm.Algebra.Classes

/- Base field -/

@[always_inline, inline]
def P: UInt32 := (15 * 2^27 + 1).toUInt32

@[always_inline, inline]
def P_U64: UInt64 := P.toUInt64

structure Elem where
  val: UInt32

@[always_inline, inline]
def M: UInt32 := 0x88000001

@[always_inline, inline]
def R2: Elem := { val := 1172168163 }

@[always_inline, inline]
def prim_mul (lhs rhs: Elem): Elem :=
  let o64: UInt64 := lhs.val.toUInt64 * rhs.val.toUInt64
  let low: UInt32 := 0 - o64.toUInt32
  let red: UInt32 := M * low
  let o64' := o64 + red.toUInt64 * P_U64
  let r := (o64' >>> 32).toUInt32
  if r >= P then { val := r - P } else { val := r }

@[always_inline, inline]
def prim_add (lhs rhs: Elem): Elem :=
  let z := lhs.val + rhs.val
  if z >= P then { val := z - P } else { val := z }

@[always_inline, inline]
def prim_sub (lhs rhs: Elem): Elem :=
  let z := lhs.val - rhs.val
  if z >= P then { val := z + P } else { val := z }

@[always_inline, inline]
def encode (a: UInt32): Elem
  := prim_mul R2 { val := a }

@[always_inline, inline]
def decode (a: Elem): UInt32
  := (prim_mul { val := 1 } a).val

@[always_inline, inline]
def BETA: Elem := encode 11

@[always_inline, inline]
def NBETA: Elem := encode (P - 11)

@[always_inline]
instance : ToString Elem where toString x := toString (decode x)

@[always_inline]
instance : Inhabited Elem where default := encode 0

@[always_inline]
instance : OfNat Elem n where ofNat := encode (UInt32.ofNat (n % P.toNat))

@[always_inline]
instance : BEq Elem where beq x y := x.val == y.val

@[always_inline]
instance : Add Elem where add x y := prim_add x y

@[always_inline]
instance : Sub Elem where sub x y := prim_sub x y

@[always_inline]
instance : Neg Elem where neg x := prim_sub 0 x

@[always_inline]
instance : Mul Elem where mul x y := prim_mul x y

@[always_inline, inline]
partial def prim_pow (x_mul: X -> X -> X) (one x: X) (n: Nat): X :=
  if n == 0 then one
  else if n == 1 then x
  else if n == 2 then x_mul x x
  else if n &&& 1 == 0 then prim_pow x_mul one (x_mul x x) (n / 2)
  else x_mul x (prim_pow x_mul one (x_mul x x) ((n - 1) / 2))

@[always_inline]
instance : HPow Elem Nat Elem where hPow x n := prim_pow prim_mul (encode 1) x n

@[always_inline]
instance : Ring Elem where ofNat n := encode (UInt32.ofNat (n % P.toNat))

@[always_inline]
instance : Div Elem where div x y := prim_mul x (prim_pow prim_mul (encode 1) y (P - 2).toNat)

@[always_inline]
instance : SerialUInt32 Elem where
  words := 1
  toUInt32Words x := #[x.val]
  fromUInt32Words x := { val := x[0]! }

@[always_inline, inline]
def prim_random {M: Type -> Type} [Monad M] [MonadRng M]: M Elem
  := do let mut val: UInt64 := 0
        for _ in [0:6] do
          let r <- MonadRng.nextUInt32
          val := ((val <<< 32) + r.toUInt64) % P_U64
        pure (encode val.toUInt32)

@[always_inline]
instance : Field Elem where
  inv x := prim_pow prim_mul (encode 1) x (P - 2).toNat
  random := prim_random
  fromUInt64 x := encode x.toUInt32

@[always_inline]
instance Elem.PrimeField : PrimeField Elem where
  toNat x := (decode x).toNat

@[always_inline]
instance Elem.RootsOfUnity : RootsOfUnity Elem where
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


structure ExtElem where
  c0: Elem
  c1: Elem
  c2: Elem
  c3: Elem
  deriving BEq

@[always_inline, inline]
def ExtElem.new (c0 c1 c2 c3: Elem): ExtElem
  := {
    c0,
    c1,
    c2,
    c3
  }

@[always_inline]
instance : ToString ExtElem where toString x := toString #[x.c0, x.c1, x.c2, x.c3]

@[always_inline]
instance : Inhabited ExtElem where default := ExtElem.new Inhabited.default Inhabited.default Inhabited.default Inhabited.default

@[always_inline, inline]
def prim_ext_of_base (c0: Elem): ExtElem
  := {
    c0,
    c1 := encode 0,
    c2 := encode 0,
    c3 := encode 0,
  }

@[always_inline, inline]
def prim_ext_of_nat (n: Nat): ExtElem
  := prim_ext_of_base (encode n.toUInt32)

@[always_inline]
instance : OfNat ExtElem n where ofNat := prim_ext_of_nat n

@[always_inline]
instance : Add ExtElem where add x y := {
  c0 := x.c0 + y.c0,
  c1 := x.c1 + y.c1,
  c2 := x.c2 + y.c2,
  c3 := x.c3 + y.c3
}

@[always_inline]
instance : Neg ExtElem where neg x := {
  c0 := - x.c0,
  c1 := - x.c1,
  c2 := - x.c2,
  c3 := - x.c3
}

@[always_inline]
instance : Sub ExtElem where sub x y := {
  c0 := x.c0 - y.c0,
  c1 := x.c1 - y.c1,
  c2 := x.c2 - y.c2,
  c3 := x.c3 - y.c3
}

@[always_inline, inline]
def prim_ext_mul (a b: ExtElem): ExtElem
  := {
    c0 := a.c0 * b.c0 + NBETA * (a.c1 * b.c3 + a.c2 * b.c2 + a.c3 * b.c1),
    c1 := a.c0 * b.c1 + a.c1 * b.c0 + NBETA * (a.c2 * b.c3 + a.c3 * b.c2),
    c2 := a.c0 * b.c2 + a.c1 * b.c1 + a.c2 * b.c0 + NBETA * (a.c3 * b.c3),
    c3 := a.c0 * b.c3 + a.c1 * b.c2 + a.c2 * b.c1 + a.c3 * b.c0
  }

@[always_inline]
instance : Mul ExtElem where mul x y := prim_ext_mul x y

@[always_inline]
instance : HPow ExtElem Nat ExtElem where hPow x n := prim_pow prim_ext_mul (prim_ext_of_nat 1) x n

@[always_inline]
instance : Ring ExtElem where ofNat n := prim_ext_of_nat n

@[always_inline, inline]
def prim_ext_inv (a: ExtElem): ExtElem :=
  let b0 := a.c0 * a.c0 + BETA * (a.c1 * (a.c3 + a.c3) - a.c2 * a.c2)
  let b2 := a.c0 * (a.c2 + a.c2) - a.c1 * a.c1 + BETA * (a.c3 * a.c3)
  let c := b0 * b0 + BETA * b2 * b2
  let ic := Field.inv c
  let b0' := b0 * ic;
  let b2' := b2 * ic;
  {
    c0 := a.c0 * b0' + BETA * a.c2 * b2',
    c1 := -a.c1 * b0' + NBETA * a.c3 * b2',
    c2 := -a.c0 * b2' + a.c2 * b0',
    c3 := a.c1 * b2' - a.c3 * b0'
  }

@[always_inline]
instance : Div ExtElem where div x y := prim_ext_mul x (prim_ext_inv y)

@[always_inline]
instance : SerialUInt32 ExtElem where
  words := 4
  toUInt32Words x := #[x.c0.val, x.c1.val, x.c2.val, x.c3.val]
  fromUInt32Words x := {
    c0 := { val := x[0]! },
    c1 := { val := x[1]! },
    c2 := { val := x[2]! },
    c3 := { val := x[3]! }
  }

@[always_inline]
instance ExtElem.Field : Field ExtElem where
  inv x := prim_ext_inv x
  random
    := do let c0: Elem <- Field.random
          let c1: Elem <- Field.random
          let c2: Elem <- Field.random
          let c3: Elem <- Field.random
          pure { c0, c1, c2, c3 }
  fromUInt64 x := prim_ext_of_nat x.toNat


/- The extension is an algebra over the base -/
@[always_inline]
instance ExtElem.Elem.Algebra : Algebra Elem ExtElem where
  ofBase c := prim_ext_of_base c
  ofBasis i x
    := match i with
        | 0 => { c0 := x, c1 := 0, c2 := 0, c3 := 0 }
        | 1 => { c0 := 0, c1 := x, c2 := 0, c3 := 0 }
        | 2 => { c0 := 0, c1 := 0, c2 := x, c3 := 0 }
        | 3 => { c0 := 0, c1 := 0, c2 := 0, c3 := x }
        | _ => 0

@[always_inline]
instance ExtElem.RootsOfUnity : RootsOfUnity ExtElem where
  MAX_ROU_SIZE := Elem.RootsOfUnity.MAX_ROU_SIZE
  ROU_FWD := Array.map Algebra.ofBase Elem.RootsOfUnity.ROU_FWD
  ROU_REV := Array.map Algebra.ofBase Elem.RootsOfUnity.ROU_REV

@[always_inline]
instance ExtElem.Elem.ExtField : ExtField Elem ExtElem where
  EXT_DEG := 4
  ofSubelems x := ExtElem.new x[0]! x[1]! x[2]! x[3]!


end Zkvm.Algebra.BabyBear
