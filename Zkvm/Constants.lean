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

namespace Zkvm.Constants

open R0sy.Lean.Nat

def MIN_CYCLES_PO2: Nat := 10

def MIN_CYCLES: Nat := 1 <<< MIN_CYCLES_PO2 -- 1K

def MAX_CYCLES_PO2: Nat := 24

def MAX_CYCLES: Nat := 1 <<< MAX_CYCLES_PO2 -- 16M

/- ~100 bits of conjectured security -/
def QUERIES: Nat := 50

def ZK_CYCLES: Nat := QUERIES

def MIN_PO2: Nat := Nat.log2_ceil (1 + ZK_CYCLES)

def INV_RATE: Nat := 4

def FRI_FOLD_PO2: Nat := 4

def FRI_FOLD: Nat := 1 <<< FRI_FOLD_PO2

def FRI_MIN_DEGREE: Nat := 256

end Zkvm.Constants
