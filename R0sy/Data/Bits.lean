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
