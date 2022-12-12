/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace RiscV.Exception

inductive RiscVException where
  | PtrOutOfBounds (addr: UInt32)
  | InstructionAddressMisaligned (addr: UInt32)

end RiscV.Exception
