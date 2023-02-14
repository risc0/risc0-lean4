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
