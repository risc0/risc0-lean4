/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace R0sy.Lean.Nat

/- Endian helpers -/

def Nat.to_be64 (x: Nat): ByteArray := {
  data := #[
    UInt8.ofNat ((x >>> (8*7)) &&& 0xff),
    UInt8.ofNat ((x >>> (8*6)) &&& 0xff),
    UInt8.ofNat ((x >>> (8*5)) &&& 0xff),
    UInt8.ofNat ((x >>> (8*4)) &&& 0xff),
    UInt8.ofNat ((x >>> (8*3)) &&& 0xff),
    UInt8.ofNat ((x >>> (8*2)) &&& 0xff),
    UInt8.ofNat ((x >>> (8*1)) &&& 0xff),
    UInt8.ofNat (x &&& 0xff)
  ]
}

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

end R0sy.Lean.Nat
