/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace R0sy.Lean.UInt64

def UInt64.lo (x: UInt64): UInt32
  := x.toNat.toUInt32

def UInt64.hi (x: UInt64): UInt32
  := (x >>> 32).toNat.toUInt32

end R0sy.Lean.UInt64
