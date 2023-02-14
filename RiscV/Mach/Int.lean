/-
 Copyright 2023 RISC Zero, Inc.

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
-/

import R0sy

namespace RiscV.Mach.Int

open R0sy.Data.Bits
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

def UInt32.toInt (x: UInt32): Int
  := if UInt32.is_neg x then Int.negOfNat (x * neg_one).toNat else x.toNat

def UInt32.lt_signed (x y: UInt32): Bool
  := UInt32.toInt x < UInt32.toInt y

def UInt32.ge_signed (x y: UInt32): Bool
  := UInt32.toInt x >= UInt32.toInt y

def UInt32.ofUInt8_signed (x: UInt8): UInt32
  := Id.run do
    let lo: Bits 7 0 := { val := x.toNat.toUInt32 }
    let hi: Bits 31 8 := Bits.ofUInt32 <| if UInt32.test_bit 7 lo.val then 0xffffffff else 0
    pure (hi.toUInt32 ||| lo.toUInt32)

def UInt32.ofUInt16_signed (x: UInt16): UInt32
  := Id.run do
    let lo: Bits 15 0 := { val := x.toNat.toUInt32 }
    let hi: Bits 31 16 := Bits.ofUInt32 <| if UInt32.test_bit 15 lo.val then 0xffffffff else 0
    pure (hi.toUInt32 ||| lo.toUInt32)

def UInt32.shr_signed (x y: UInt32): UInt32
  := Id.run do
      let lo := x >>> y
      let hi := if UInt32.is_neg x then ((1 <<< y) - 1) <<< (32 - y) else 0
      pure (hi ||| lo)

end RiscV.Mach.Int
