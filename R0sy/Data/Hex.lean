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

namespace R0sy.Data.Hex

def charTable: Array Char
  := #[
    '0', '1', '2', '3',
    '4', '5', '6', '7',
    '8', '9', 'a', 'b',
    'c', 'd', 'e', 'f'
  ]

def UInt8.toHex (val: UInt8): String
  := Id.run do
        let lo := (val &&& 0x0f).toNat
        let hi := (val >>> 4).toNat
        let mut out: String := ""
        out := out.push charTable[hi]!
        out := out.push charTable[lo]!
        out

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
