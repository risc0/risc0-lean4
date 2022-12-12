/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import R0sy.Algebra.Poly
import R0sy.Hash
import R0sy.Serial

namespace R0sy.Algebra.Field.BabyBear2

open Hash
open Poly
open Serial

/- Base field -/

def P: UInt32 := (15 * 2^27 + 1).toUInt32
def P_U64: UInt64 := P.toUInt64
def RANDOM_CUTOFF := (0xffffffff / P) * P

structure Elem where
  val: UInt32

def M: UInt32 := 0x88000001
def R2: Elem := { val := 1172168163 }

def prim_mul (lhs rhs: Elem): Elem :=
  let o64: UInt64 := lhs.val.toUInt64 * rhs.val.toUInt64
  let low: UInt32 := 0 - o64.toUInt32
  let red: UInt32 := M * low
  let o64' := o64 + red.toUInt64 * P_U64
  let r := (o64' >>> 32).toUInt32
  if r >= P then { val := r - P } else { val := r }

def prim_add (lhs rhs: Elem): Elem :=
  let z := lhs.val + rhs.val
  if z >= P then { val := z - P } else { val := z }

def prim_sub (lhs rhs: Elem): Elem :=
  let z := lhs.val - rhs.val
  if z >= P then { val := z + P } else { val := z }

def encode (a: UInt32): Elem
  := prim_mul R2 { val := a }

def decode (a: Elem): UInt32
  := (prim_mul { val := 1 } a).val

instance : ToString Elem where toString x := toString x.val

instance : Inhabited Elem where default := encode 0

instance : OfNat Elem n where ofNat := encode (UInt32.ofNat (n % P.toNat))

instance : BEq Elem where beq x y := x.val == y.val

instance : Add Elem where add x y := prim_add x y

instance : Sub Elem where sub x y := prim_sub x y

instance : Neg Elem where neg x := prim_sub 0 x

instance : Mul Elem where mul x y := prim_mul x y

partial def prim_pow (x_mul: X -> X -> X) (one x: X) (n: Nat): X :=
  if n == 0 then one
  else if n == 1 then x
  else if n == 2 then x_mul x x
  else if n &&& 1 == 0 then prim_pow x_mul one (x_mul x x) (n / 2)
  else x_mul x (prim_pow x_mul one (x_mul x x) ((n - 1) / 2))

instance : HPow Elem Nat Elem where hPow x n := prim_pow prim_mul (encode 1) x n

instance : Ring Elem where ofNat n := encode (UInt32.ofNat (n % P.toNat))

instance : Div Elem where div x y := prim_mul x (prim_pow prim_mul (encode 1) y (P - 2).toNat)

instance : SerialUInt32 Elem where
  words := 1
  toUInt32Words x := #[x.val]
  fromUInt32Words x := { val := x[0]! }

partial def prim_random {M: Type -> Type} [Monad M] [MonadRng M]: M Elem
  := do let x <- MonadRng.nextUInt32
        if x >= RANDOM_CUTOFF
          then prim_random
          else return encode x

instance : Field Elem where
  inv x := prim_pow prim_mul (encode 1) x (P - 2).toNat
  random := prim_random
  fromUInt64 x := encode x.toUInt32

instance Elem.PrimeField : PrimeField Elem where
  toNat x := (decode x).toNat

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

#eval
  let x: Elem := RootsOfUnity.ROU_FWD[15]!
  PrimeField.toNat (x ^ (2 ^ 15))

end R0sy.Algebra.Field.BabyBear2
