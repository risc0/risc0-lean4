/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace RiscV.Config

inductive Xlen: Type where
  | Xlen32
  | Xlen64

def Xlen.bits (xlen: Xlen): Nat
  := match xlen with
      | .Xlen32 => 32
      | .Xlen64 => 64

inductive Endian: Type where
  | Big
  | Little

inductive InstrSet: Type where
  | RV32IM

structure Config where
  xlen: Xlen
  endian: Endian
  instrSet: InstrSet

def RV32IMle: Config
  := {
    xlen := .Xlen32
    endian := .Little
    instrSet := .RV32IM
  }

end RiscV.Config
