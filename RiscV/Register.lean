/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace RiscV.Register

def X_REGISTER_COUNT: Nat := 32

inductive Register where
  | X (reg: Fin X_REGISTER_COUNT)
  | PC

def Register.ofNat (val: Nat): Register
  := if isLt: val < X_REGISTER_COUNT
      then Register.X { val, isLt }
      else Register.PC

def Register.ofUInt32 (x: UInt32): Register
  := Register.ofNat x.toNat

def Register.index (self: Register): Nat
  := match self with
      | X x => x.val
      | PC => X_REGISTER_COUNT


structure RegisterFile where
  data: Array UInt32

def RegisterFile.new: RegisterFile
  := {
    data := Array.mkArray (X_REGISTER_COUNT + 1) 0
  }

def RegisterFile.get_word [Monad M] [MonadStateOf RegisterFile M] (reg: Register): M UInt32
  := do let self <- get
        pure <| self.data[reg.index]!

def RegisterFile.set_word [Monad M] [MonadStateOf RegisterFile M] (reg: Register) (val: UInt32): M Unit
  := do let self <- get
        set {
          self with
          data := Array.setD self.data reg.index val
        }


end RiscV.Register
