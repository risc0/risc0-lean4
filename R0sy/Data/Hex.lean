/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace R0sy.Data.Hex

def charTable: Array String
  := #[
    "0", "1", "2", "3",
    "4", "5", "6", "7",
    "8", "9", "a", "b",
    "c", "d", "e", "f"
  ]

def UInt8.toHex (val: UInt8): String
  := Id.run do
        let lo := (val &&& 0x0f).toNat
        let hi := (val >>> 4).toNat
        charTable[hi]! ++ charTable[lo]!

def UInt16.toHex (val: UInt16): String
  := Id.run do
        let lo := UInt8.toHex (val &&& 0xff).toNat.toUInt8
        let hi := UInt8.toHex (val >>> 8 &&& 0xff).toNat.toUInt8
        hi ++ lo

def UInt32.toHex (val: UInt32): String
  := Id.run do
        let lo := UInt16.toHex (val &&& 0xffff).toNat.toUInt16
        let hi := UInt16.toHex (val >>> 16 &&& 0xffff).toNat.toUInt16
        hi ++ lo

end R0sy.Data.Hex
