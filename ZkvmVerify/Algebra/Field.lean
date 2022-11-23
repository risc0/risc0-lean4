/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import ZkvmVerify.Algebra

namespace ZkvmVerify.Algebra.Field


namespace Prime

/- Field elements -/

structure Elem (p: Prime) where
  rep: Fin p.rep
  deriving Repr


/- BEq -/

def Elem.beq (x y: Elem p): Bool := x.rep.val == y.rep.val

instance : BEq (Elem p) where beq := Elem.beq


/- OfNat -/

def Elem.ofNat (p: Prime) (n: Nat): Elem p := {
  rep := Fin.ofNat' n p.pos
}

instance : OfNat (Elem p) n where ofNat := Elem.ofNat p n

def Elem.ofUInt32 (n: UInt32): Elem p := Elem.ofNat _ (UInt32.toNat n)


/- Add -/

def Elem.add (x y: Elem p): Elem p := { rep := x.rep + y.rep }

instance : Add (Elem p) where add := Elem.add


/- Neg -/

def Elem.neg (x: Elem p): Elem p := { rep := Fin.ofNat' 0 p.pos - x.rep }

instance : Neg (Elem p) where neg := Elem.neg


/- Sub -/

def Elem.sub (x y: Elem p): Elem p := { rep := x.rep - y.rep }

instance : Sub (Elem p) where sub := Elem.sub


/- Mul -/

def Elem.mul (x y: Elem p): Elem p := { rep := x.rep * y.rep }

instance : Mul (Elem p) where mul := Elem.mul


/- Pow -/

partial def Elem.pow (x: Elem p) (n: Nat): Elem p :=
  if n == 0 then 1
  else if n == 1 then x
  else if n == 2 then x * x
  else if n &&& 1 == 0 then pow (x * x) (n / 2)
  else x * pow (x * x) ((n - 1) / 2)

instance : HPow (Elem p) Nat (Elem p) where hPow := Elem.pow


/- Div -/

def Elem.inv (x: Elem p): Elem p := x ^ (p.rep - 2)

def Elem.div (x y: Elem p): Elem p := x * y.inv

instance : Div (Elem p) where div := Elem.div


/- Ring and Field -/

instance : Ring (Elem p) where
  ofNat := Elem.ofNat p

instance : Field (Elem p) where
  inv := Elem.inv

end Prime


namespace Ext

/- Field elements -/

structure Elem (q: Irreducible F R) where
  rep: R
  deriving Repr

def Elem.beq [BEq R] {q: Irreducible F R} (x y: Elem q): Bool := x.rep == y.rep

instance [BEq R] {q: Irreducible F R} : BEq (Elem q) where beq := Elem.beq


/- OfNat -/

def Elem.ofBase [PolyRing F R] (q: Irreducible F R) (n: F): Elem q := {
  rep := PolyRing.mono 0 n
}

def Elem.ofNat [Ring F] [PolyRing F R] (q: Irreducible F R) (n: Nat): Elem q := Elem.ofBase _ (Ring.ofNat n)

instance [Ring F] [PolyRing F R] {q: Irreducible F R} : OfNat (Elem q) n where ofNat := Elem.ofNat _ n

def Elem.ofUInt32 [Ring F] [PolyRing F R] (q: Irreducible F R) (n: UInt32): Elem q := Elem.ofNat _ (UInt32.toNat n)


/- Add -/

def Elem.add [Ring R] {q: Irreducible F R} (x y: Elem q): Elem q := {
  rep := x.rep + y.rep
}

instance [Ring R] {q: Irreducible F R} : Add (Elem q) where add := Elem.add


/- Neg -/

def Elem.neg [Ring R] {q: Irreducible F R} (x: Elem q): Elem q := {
  rep := - x.rep
}

instance [Ring R] {q: Irreducible F R} : Neg (Elem q) where neg := Elem.neg


/- Sub -/

def Elem.sub [Ring R] {q: Irreducible F R} (x y: Elem q): Elem q := {
  rep := x.rep - y.rep
}

instance [Ring R] {q: Irreducible F R} : Sub (Elem q) where sub := Elem.sub


/- Mul -/

def Elem.mul [Ring R] [DivRemRing R] {q: Irreducible F R} (x y: Elem q): Elem q := {
  rep := x.rep * y.rep % q.rep
}

instance [Ring R] [DivRemRing R] {q: Irreducible F R} : Mul (Elem q) where mul := Elem.mul


/- Pow -/

partial def Elem.pow [Ring F] [Ring R] [PolyRing F R] [DivRemRing R] {q: Irreducible F R} (x: Elem q) (n: Nat): Elem q :=
  if n == 0 then 1
  else if n == 1 then x
  else if n == 2 then x * x
  else if n &&& 1 == 0 then pow (x * x) (n / 2)
  else x * pow (x * x) ((n - 1) / 2)

instance [Ring F] [Ring R] [PolyRing F R] [DivRemRing R] {q: Irreducible F R} : HPow (Elem q) Nat (Elem q) where hPow := Elem.pow


/- Div -/

def Elem.inv [Ring F] [PolyRing F R] [Ring R] [GcdRing R] {q: Irreducible F R} (x: Elem q): Elem q :=
  if x == 0 then 0
  else {
    rep := (GcdRing.gcd x.rep q.rep).s
  }

def Elem.div [Ring F] [Ring R] [PolyRing F R] [DivRemRing R] [GcdRing R] {q: Irreducible F R} (x y: Elem q): Elem q := x * y.inv

instance [Ring F] [Ring R] [PolyRing F R] [DivRemRing R] [GcdRing R] {q: Irreducible F R} : Div (Elem q) where div := Elem.div


/- Ring and Field -/

instance [Field F] [Ring R] [PolyRing F R] [DivRemRing R] [GcdRing R] {q: Irreducible F R} : Ring (Elem q) where
  ofNat := Elem.ofNat _

instance [Field F] [Ring R] [PolyRing F R] [DivRemRing R] [GcdRing R] {q: Irreducible F R} : Field (Elem q) where
  inv := Elem.inv

instance [Field F] [Ring R] [PolyRing F R] [DivRemRing R] [GcdRing R] {q: Irreducible F R} : Algebra F (Elem q) where
  ofBase := Elem.ofBase _

end Ext

end ZkvmVerify.Algebra.Field

