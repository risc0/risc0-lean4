/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace RiscV.Mach.Exception

inductive RiscVException where
  | PtrOutOfBounds (addr: Nat)
  | InstructionAddressMisaligned (addr: Nat)
  | InvalidInstruction (addr instr: Nat)
  | ECall (ecall: Nat)

instance : ToString RiscVException where
  toString
  | .PtrOutOfBounds addr => s!"PtrOutOfBounds addr:{addr}"
  | .InstructionAddressMisaligned addr => s!"InstructionAddressMisaligned addr:{addr}"
  | .InvalidInstruction addr instr => s!"InvalidInstruction addr:{addr} instr:{instr}"
  | .ECall addr => s!"ECall addr:{addr}"

end RiscV.Mach.Exception
