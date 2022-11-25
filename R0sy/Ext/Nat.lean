/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace R0sy.Ext.Nat

/- Serialize/deserialize helpers -/

def Nat.toUInt32Words (words: Nat) (val: Nat) (out: Array UInt32 := #[]): Array UInt32
  := match words with
      | 0 => out
      | words + 1 => Nat.toUInt32Words words (val >>> 32) (out.push (UInt32.ofNat val))

def Nat.fromUInt32Words (x: Subarray UInt32) (i: Nat := 0) (out: Nat := 0): Nat
  := if i < x.size
      then Nat.fromUInt32Words x (i + 1) ((out <<< 32) ||| UInt32.toNat x[x.size - i - 1]!)
      else out
termination_by _ => x.size - i

end R0sy.Ext.Nat
