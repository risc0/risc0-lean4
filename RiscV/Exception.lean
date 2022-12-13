/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace RiscV.Exception

inductive RiscVException where
  | PtrOutOfBounds (addr: UInt32)
  | InstructionAddressMisaligned (addr: UInt32)
  | InvalidInstruction (addr instr: UInt32)

instance : ToString RiscVException where
  toString
  | .PtrOutOfBounds addr => s!"PtrOutOfBounds addr:{addr}"
  | .InstructionAddressMisaligned addr => s!"InstructionAddressMisaligned addr:{addr}"
  | .InvalidInstruction addr instr => s!"InvalidInstruction addr:{addr} instr:{instr}"

end RiscV.Exception
