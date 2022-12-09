/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra

namespace R0sy.Algebra.Poly

open Algebra


/- Polynomials -/

structure Poly (R: Type) where
  rep: Array R
  deriving Repr

instance [ToString R] : ToString (Poly R) where toString x := toString x.rep

def Poly.ofArray [Ring R] (rep: Array R): Poly R := { rep }

def Poly.ofSubarray [Ring R] (rep: Subarray R): Poly R := { rep }

def Poly.zero [Ring R]: Poly R := {
  rep := #[]
}

instance [Ring R] : Inhabited (Poly R) where default := Poly.zero

instance [Ring R] : OfNat (Poly R) 0 where ofNat := Poly.zero

def Poly.one [Ring R]: Poly R := {
  rep := #[Ring.one]
}

instance [Ring R] : OfNat (Poly R) 1 where ofNat := Poly.one


/- Size and degree -/

def Poly.required_size [Ring R] (x: Poly R): Nat
 := let rec f (i: Nat) :=
      match i with
      | 0 => 0
      | i' + 1 =>
          if Array.getD x.rep i' Ring.zero == Ring.zero
          then f i'
          else i
    f x.rep.size

def Poly.deg [Ring R] (x: Poly R): Nat := x.required_size - 1

def Poly.normalize [Ring R] (x: Poly R): Poly R :=
  if x.rep.size == x.required_size
    then x
    else  {
      rep := Array.ofSubarray (Array.toSubarray x.rep 0 x.required_size)
    }


/- Coefficients and monomials -/

def Poly.coeff [Ring R] (x: Poly R) (i: Nat): R := Array.getD x.rep i Ring.zero

def Poly.lead_coeff [Ring R] (x: Poly R): R := x.coeff x.deg

def Poly.mono [Ring R] (deg: Nat) (c: R): Poly R :=
  let out := mkArray (deg + 1) Ring.zero
  {
    rep := Array.setD out deg c
  }


/- BEq -/

partial def Poly.beqAux [Ring R] (xx yy: Array R) (i: Nat): Bool :=
  if i < max xx.size yy.size
  then
    let a := Array.getD xx i Ring.zero
    let b := Array.getD yy i Ring.zero
    if a == b
      then beqAux xx yy (i + 1)
      else false
  else true

def Poly.beq [Ring R] (x y: Poly R): Bool :=
  if x.deg != y.deg then false
  else beqAux x.rep y.rep 0

instance [Ring R] : BEq (Poly R) where beq := Poly.beq


/- Add -/

partial def Poly.addAux [Ring R] (xx yy: Array R) (i: Nat): Array R :=
  if h: i < xx.size
    then
      let a: R := xx[i]
      let b: R := Array.getD yy i Ring.zero
      addAux (Array.setD xx i (a + b)) yy (i + 1)
    else xx
-- termination_by _ => xx.size - i

def Poly.add [Ring R] (x y: Poly R): Poly R :=
  if x == 0 then y
  else if y == 0 then x
  else {
    rep :=
      if x.rep.size > y.rep.size
        then addAux x.rep y.rep 0
        else addAux y.rep x.rep 0
  }

instance [Ring R] : Add (Poly R) where add := Poly.add


/- Neg -/

partial def Poly.negAux [Ring R] (xx: Array R) (i: Nat): Array R :=
  if h: i < xx.size
    then
      let a: R := xx[i]
      negAux (Array.setD xx i (- a)) (i + 1)
    else xx
-- termination_by _ => xx.size - i

def Poly.neg [Ring R] (x: Poly R): Poly R := {
  rep := negAux x.rep 0
}

instance [Ring R] : Neg (Poly R) where neg := Poly.neg


/- Sub -/

def Poly.sub [Ring R] (x y: Poly R): Poly R := x + (- y)

instance [Ring R] : Sub (Poly R) where sub := Poly.sub


/- Scale -/

partial def Poly.scaleAux [Ring R] (s: R) (xx: Array R) (i: Nat): Array R :=
  if h: i < xx.size
    then
      let a: R := xx[i]
      scaleAux s (Array.setD xx i (s * a)) (i + 1)
    else xx
-- termination_by _ => xx.size - i

def Poly.scale [Ring R] (s: R) (x: Poly R): Poly R := {
  rep := scaleAux s x.rep 0
}


/- Mul -/

def Poly.mul [Ring R] (x y: Poly R): Poly R
  := Id.run do
    let mut out: Poly R := Poly.zero
    for i in [0:Poly.deg x + 1] do
      let c := Poly.coeff x i
      if c != Ring.zero then out := out + { rep := Array.mkArray i Ring.zero ++ (Poly.scale c y).rep }
    pure out

instance [Ring R] : Mul (Poly R) where mul := Poly.mul


/- Pow -/

partial def Poly.pow [Ring R] (x: Poly R) (n: Nat): Poly R :=
  if n == 0 then Poly.one
  else if n == 1 then x
  else if n == 2 then x * x
  else if n &&& 1 == 0 then pow (x * x) (n / 2)
  else x * pow (x * x) ((n - 1) / 2)

instance [Ring R] : HPow (Poly R) Nat (Poly R) where hPow := Poly.pow


/- Div -/

partial def Poly.divAux [Field R] (n d q r: Poly R): (Poly R × Poly R) :=
  if r == 0 || r.deg < d.deg
    then (q, r)
    else
      let t := Poly.mono (r.deg - d.deg) (r.lead_coeff / d.lead_coeff)
      let q' := q + t
      let r' := r - t * d
      divAux n d q' r'

def Poly.div [Field R] (n d: Poly R): Poly R :=
  if d == 0 then panic! "divide by zero!"
  else if n == 0 then 0
  else (divAux n d Poly.zero n).1.normalize

instance [Field R] : Div (Poly R) where div := Poly.div


/- Mod -/

def Poly.mod [Field R] (n d: Poly R): Poly R :=
  if d == 0 then panic! "divide by zero!"
  else if n == 0 then 0
  else (divAux n d Poly.zero n).2.normalize

instance [Field R] : Mod (Poly R) where mod := Poly.mod


/- Subst -/

def Poly.substAux [Ring R] (xx: Array R) (y: Poly R) (ypow: Poly R) (i: Nat): Poly R :=
  if h: i < xx.size
  then substAux xx y (y * ypow) (i + 1) + (Poly.mono 0 xx[i]) * ypow
  else 0
termination_by _ => xx.size - i

def Poly.subst [Ring R] (x y: Poly R): Poly R := substAux x.rep y 1 0


/- Eval -/

def Poly.eval [Ring R] (poly: Poly R) (x: R): R -- := (Poly.subst x (Poly.mono 0 y)).coeff 0
  := Id.run do
    let mut mul_x: R := Ring.one
    let mut tot: R := Ring.zero
    for i in [0:Poly.deg poly + 1] do
      tot := tot + (Poly.coeff poly i * mul_x)
      mul_x := mul_x * x
    pure tot


/- GCD -/

partial def Poly.gcdAux [Field R] (gcd0 gcd1: Gcd (Poly R)): Gcd (Poly R) :=
  if gcd1.r == 0
    then
      let scale := Poly.mono 0 (Field.inv (gcd0.r.coeff 0))
      {
        r := scale * gcd0.r,
        s := scale * gcd0.s,
        t := scale * gcd0.t,
      }
    else
      let q: Poly R := gcd0.r / gcd1.r
      gcdAux gcd1
      {
        r := (gcd0.r - q * gcd1.r).normalize,
        s := (gcd0.s - q * gcd1.s).normalize,
        t := (gcd0.t - q * gcd1.t).normalize,
      }

def Poly.gcd [Field R] (a b: Poly R): Gcd (Poly R) :=
  gcdAux
    {
      r := a.normalize,
      s := 1,
      t := 0
    }
    {
      r := b.normalize,
      s := 0,
      t := 1
    }

instance [Field R] : GcdRing (Poly R) where gcd := Poly.gcd


/- Algebra, Ring, PolyRing, and DivRemRing -/

instance [Ring R] : Ring (Poly R) where
  ofNat n := Poly.mono 0 (Ring.ofNat n)

instance [Ring R] : Algebra R (Poly R) where
  ofBase f := Poly.mono 0 f
  ofBasis := Poly.mono

instance [Ring R] : PolyRing R (Poly R) where
  coeff := Poly.coeff
  deg := Poly.deg
  mono := Poly.mono
  eval := Poly.eval
  subst := Poly.subst
  ofArray := Poly.ofArray

instance [Field R] : DivRemRing (Poly R) where

instance [Field R] : GcdRing (Poly R) where
  gcd := Poly.gcd

end R0sy.Algebra.Poly
