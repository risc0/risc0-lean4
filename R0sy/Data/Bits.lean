/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace R0sy.Data.Bits

def Bits.least_upper_bound (hi lo: UInt32): Nat
  := 1 <<< (1 + hi.toNat - lo.toNat)

def Bits.mask (hi lo: UInt32): UInt32
  := (Bits.least_upper_bound hi lo - 1).toUInt32 <<< lo

structure Bits (hi lo: UInt32) where
  val: UInt32 -- Fin (Bits.least_upper_bound hi lo)
  deriving BEq

def Bits.ofUInt32 (x: UInt32): Bits hi lo
  := {
    val := (x >>> lo) &&& (Bits.least_upper_bound hi lo - 1).toUInt32
  }

def Bits.toUInt32 (x: Bits hi lo): UInt32
  := x.val <<< lo

end R0sy.Data.Bits
