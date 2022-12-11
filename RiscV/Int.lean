/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy

namespace RiscV.Int

open R0sy.Lean.UInt32

def UInt32.is_neg (x: UInt32): Bool
  := UInt32.test_bit 31 x

def UInt32.extend_unsigned (x: UInt32): UInt64
  := x.toNat.toUInt64

def UInt32.extend_signed (x: UInt32): UInt64
  := UInt32.extend_unsigned x ||| if UInt32.is_neg x then 0xffffffff00000000 else 0

def UInt32.neg_one: UInt32 := 0xffffffff

def UInt32.max_signed: UInt32 := 0x7fffffff

def UInt32.min_signed: UInt32 := 0x80000000

def UInt32.max_unsigned: UInt32 := 0xffffffff

def UInt32.min_unsigned: UInt32 := 0x00000000

end RiscV.Int
