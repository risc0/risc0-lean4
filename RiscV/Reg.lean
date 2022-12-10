/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace RiscV.Reg

def X_REGISTER_COUNT: Nat := 32

inductive Reg where
  | X (reg: Fin X_REGISTER_COUNT)
  | PC

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

def RegFile.new: RegFile
  := {
    data := Array.mkArray (X_REGISTER_COUNT + 1) 0
  }

def RegFile.get_word [Monad M] [MonadStateOf RegFile M] (reg: Reg): M UInt32
  := do let self <- get
        pure <| self.data[reg.index]!

def RegFile.set_word [Monad M] [MonadStateOf RegFile M] (reg: Reg) (val: UInt32): M Unit
  := do let self <- get
        set {
          self with
          data := Array.setD self.data reg.index val
        }


end RiscV.Reg
