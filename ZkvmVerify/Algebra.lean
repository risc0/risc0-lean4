/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace ZkvmVerify.Algebra

/- Prime numbers -/

structure Prime where
  rep: Nat
  pos: p > 0
  deriving Repr

def Prime.beq (x y: Prime): Bool := x.rep == y.rep

instance : BEq Prime where beq := Prime.beq


/- Irreducible polynomials -/

structure Irreducible (F R: Type) where
  rep: R
  deriving Repr

def Irreducible.beq [BEq R] (x y: Irreducible F R): Bool := x.rep == y.rep

instance [BEq R] : BEq (Irreducible F R) where beq := Irreducible.beq


/- Algebra -/

class Ring (R: Type)
  extends
    BEq R,
    Add R,
    Neg R,
    Sub R,
    Mul R,
    HPow R Nat R
  where
    ofNat: Nat -> R
    zero: R := ofNat 0
    one: R := ofNat 1

class Field (F: Type)
  extends
    Ring F,
    Div F
  where
    inv: F -> F

class Algebra (F: Type) (R: Type)
  where
    ofBase: F -> R

class PolyRing (F: Type) (R: Type)
  extends
    Algebra F R
  where
    coeff: R -> Nat -> F
    deg: R -> Nat
    mono: Nat -> F -> R
    eval: R -> F -> F
    subst: R -> R -> R

class DivRemRing (R: Type)
  extends
    Div R,
    Mod R

-- Extended GCD: `gcd(a, b) = r = s*a + t*b`
structure Gcd (R: Type) where
  r: R
  s: R
  t: R

class GcdRing (R: Type) where
  gcd: R -> R -> Gcd R

end ZkvmVerify.Algebra
