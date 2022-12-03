/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Lean.Nat

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

def CHECK_SIZE := INV_RATE * 4 -- TODO: field extension degree

end Zkvm.Constants
