/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace RiscV.Config

inductive Xlen: Type where
  | Xlen32
  | Xlen64

def Xlen.bits (self: Xlen): Nat
  := match self with
      | .Xlen32 => 32
      | .Xlen64 => 64

inductive Endian: Type where
  | Big
  | Little

end RiscV.Config
