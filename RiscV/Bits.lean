/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace RiscV.Bits

def Bits.max (hi lo: UInt32): Nat
  := 2 ^ (1 + hi.toNat - lo.toNat)

structure Bits (hi lo: UInt32) where
  val: UInt32 -- Fin (Bits.max hi lo)
  deriving BEq

def Bits.ofUInt32 (x: UInt32): Bits hi lo
  := {
    val := (x >>> lo) &&& (Bits.max hi lo - 1).toUInt32
  }

end RiscV.Bits
