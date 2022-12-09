/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import R0sy.Algebra.Field
import R0sy.Algebra.Poly
import R0sy.Serial

namespace R0sy.Algebra.Field.Goldilocks

open Poly
open Serial

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

instance : ToString Elem where toString x := toString x.rep

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
  toUInt32Words x := SerialUInt32.toUInt32Words x.rep
  fromUInt32Words x := { rep := SerialUInt32.fromUInt32Words x }

instance : Field Elem where
  inv x := { rep := x.rep.inv }
  random
    := do let rep <- Prime.Elem.random P
          return { rep }
  fromUInt64 x := { rep := Field.fromUInt64 x }

instance Elem.PrimeField : PrimeField Elem where
  toNat x := PrimeField.toNat x.rep

instance Elem.RootsOfUnity : RootsOfUnity Elem where
  MAX_ROU_SIZE := 32
  ROU_FWD := #[
    1,
    18446744069414584320,
    281474976710656,
    18446744069397807105,
    17293822564807737345,
    70368744161280,
    549755813888,
    17870292113338400769,
    13797081185216407910,
    1803076106186727246,
    11353340290879379826,
    455906449640507599,
    17492915097719143606,
    1532612707718625687,
    16207902636198568418,
    17776499369601055404,
    6115771955107415310,
    12380578893860276750,
    9306717745644682924,
    18146160046829613826,
    3511170319078647661,
    17654865857378133588,
    5416168637041100469,
    16905767614792059275,
    9713644485405565297,
    5456943929260765144,
    17096174751763063430,
    1213594585890690845,
    6414415596519834757,
    16116352524544190054,
    9123114210336311365,
    4614640910117430873,
    1753635133440165772
  ]
  ROU_REV := #[
    1,
    18446744069414584320,
    18446462594437873665,
    1099511627520,
    68719476736,
    18446744069414322177,
    18302628881338728449,
    18442240469787213841,
    2117504431143841456,
    4459017075746761332,
    4295002282146690441,
    8548973421900915981,
    11164456749895610016,
    3968367389790187850,
    4654242210262998966,
    1553425662128427817,
    7868944258580147481,
    14744321562856667967,
    2513567076326282710,
    5089696809409609209,
    17260140776825220475,
    11898519751787946856,
    15307271466853436433,
    5456584715443070302,
    1219213613525454263,
    13843946492009319323,
    16884827967813875098,
    10516896061424301529,
    4514835231089717636,
    16488041148801377373,
    16303955383020744715,
    10790884855407511297,
    8554224884056360729
  ]


/- Extension field -/

def Q: Irreducible Elem (Poly.Poly Elem) := {
  rep := Poly.mono 2 Ring.one - Poly.mono 0 (Ring.ofNat 11)
}

structure ExtElem where
  rep: Ext.Elem Q
  deriving Repr

def ExtElem.new (a0 a1: Elem): ExtElem := {
  rep := { rep := { rep := #[a0, a1] } }
}

instance : ToString ExtElem where toString x := toString x.rep

instance : Inhabited ExtElem where default := { rep := Inhabited.default }

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

instance ExtElem.Field : Field ExtElem where
  inv x := { rep := x.rep.inv }
  random
    := do let rep <- Ext.Elem.random Q
          return { rep }
  fromUInt64 x := { rep := Field.fromUInt64 x }


/- The extension is an algebra over the base -/

instance ElemExt.Elem.Algebra : Algebra Elem ExtElem where
  ofBase c := { rep := Algebra.ofBase c }
  ofBasis i x := { rep := Algebra.ofBasis i x }

instance ElemExt.Elem.ExtField : ExtField Elem ExtElem where
  EXT_DEG := Poly.deg Q.rep
  ofSubelems x := ExtElem.new x[0]! x[1]!

/- Examples -/

#eval
  let base: Nat -> Elem := Ring.ofNat
  let to: Elem -> Array UInt32 := SerialUInt32.toUInt32Words
  let fr: Subarray UInt32 -> Elem := SerialUInt32.fromUInt32Words
  base 0xffffffaaaa == fr (to (base 0xffffffaaaa)).toSubarray

#eval
  let base: Nat -> Elem := Ring.ofNat
  let to: Elem -> Array UInt32 := SerialUInt32.toUInt32Words
  let fr: Subarray UInt32 -> Elem := SerialUInt32.fromUInt32Words
  base 0xffffffaaaa == fr (to (base 0xffffffaaaa)).toSubarray

#eval
  let x: ExtElem := { rep := { rep := { rep := #[0xffffffaaaa, 0xccccccbbbb] } } }
  let to: ExtElem -> Array UInt32 := SerialUInt32.toUInt32Words
  let fr: Subarray UInt32 -> ExtElem := SerialUInt32.fromUInt32Words
  x == fr (to x).toSubarray

#eval
  let x: Elem := RootsOfUnity.ROU_FWD[15]!
  x ^ (2 ^ 15)

end R0sy.Algebra.Field.Goldilocks
