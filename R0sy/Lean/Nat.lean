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

namespace R0sy.Lean.Nat


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


/- Log2 -/

partial def Nat.log2_ceil (value: Nat) (result: Nat := 0): Nat
  := if (1 <<< result) < value then log2_ceil value (result + 1) else result

partial def Nat.log2_floor (value: Nat) (result: Nat := 0): Nat
  := if (1 <<< (result + 1)) > value then result else log2_floor value (result + 1)

end R0sy.Lean.Nat
