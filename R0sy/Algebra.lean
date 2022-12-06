/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Hash
import R0sy.Serial

namespace R0sy.Algebra

open Hash
open Serial

/- Prime numbers -/

structure Prime where
  rep: Nat
  words: Nat
  random_cutoff: Nat
  pos: rep > 0
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
    ToString R,
    Inhabited R,
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
    Div F,
    SerialUInt32 F
  where
    inv: F -> F
    random: [Monad M] -> [MonadRng M] -> M F
    fromUInt64: UInt64 -> F

class PrimeField (F: Type)
  extends
    Field F
  where
    toNat: F -> Nat

class ExtField (F R: Type) where
  EXT_DEG: Nat

class RootsOfUnity (F: Type) where
  MAX_ROU_SIZE: Nat
  ROU_FWD: Array F
  ROU_REV: Array F

class Algebra (F R: Type)
  where
    ofBase: F -> R
    ofBasis: Nat -> F -> R

class PolyRing (F R: Type)
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

end R0sy.Algebra
