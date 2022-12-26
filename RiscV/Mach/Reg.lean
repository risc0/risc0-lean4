/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy

namespace RiscV.Mach.Reg

open R0sy.Data.Hex

def X_REGISTER_COUNT: Nat := 32

inductive Reg where
  | X (reg: Fin X_REGISTER_COUNT)
  | PC

instance : ToString Reg where
  toString
    | .X reg => s!"x{reg.val}"
    | .PC => "PC"

def Reg.ofNat (val: Nat): Reg
  := if isLt: val < X_REGISTER_COUNT
      then Reg.X { val, isLt }
      else Reg.PC

def Reg.ofUInt32 (x: UInt32): Reg
  := Reg.ofNat x.toNat

def Reg.index (self: Reg): Nat
  := match self with
      | X x => x.val
      | PC => X_REGISTER_COUNT


structure RegFile where
  data: Array UInt32

instance : ToString RegFile where
  toString self
    := Id.run do
        let mut out := ""
        for i in [0:self.data.size] do
          let val := self.data[i]!
          if val != 0 then out := s!"{out}{Reg.ofNat i}:{UInt32.toHex val}  "
        pure out

def RegFile.new: RegFile
  := {
    data := Array.mkArray (X_REGISTER_COUNT + 1) 0
  }

def RegFile.newWithPc (pc: UInt32): RegFile
  := {
    data := Array.setD RegFile.new.data Reg.PC.index pc
  }

def RegFile.get_word [Monad M] [MonadStateOf RegFile M] (reg: Reg): M UInt32
  := do if reg.index == 0 then return 0
        let self <- get
        pure <| self.data[reg.index]!

def RegFile.set_word [Monad M] [MonadStateOf RegFile M] (reg: Reg) (val: UInt32): M Unit
  := do if reg.index == 0 then return ()
        let self <- get
        set {
          self with
          data := Array.setD self.data reg.index val
        }


end RiscV.Mach.Reg
