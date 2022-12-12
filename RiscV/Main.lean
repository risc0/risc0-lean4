/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV

def main : IO Unit
  := do IO.println "riscv emu!"
        let machine: RiscV.Monad.Machine := {
          reg_file := RiscV.Reg.RegFile.new
          mem := {
            blocks := #[
              {
                base := { val := 0x00000000 },
                data := #[
                  0x00400993,   /- li s3,4 -/
                  0x00000a13,   /- li s4,0 -/
                  0x00100913,   /- li s2,1 -/
                  0x00000493,   /- li s1,0 -/
                  0x0140006f,   /- j 101e0 <main+0x44> -/
                  0x012a0ab3,   /- add s5,s4,s2 -/
                  0x00090a13,   /- mv s4,s2 -/
                  0x000a8913,   /- mv s2,s5 -/
                  0x00148493,   /- addi s1,s1,1 -/
                  0xff34c8e3    /- blt s1,s3,101d0 <main+0x34> -/
                ]
              }
            ]
          }
        }
        let step {M: Type -> Type} [RiscV.Monad.MonadMachine M]: M Unit := RiscV.Instr.Types.InstructionSet.step RiscV.Instr.RV32IM.RV32IM
        let (result, machine') := RiscV.Monad.MonadMachine.run machine do
          for _ in [0:50] do
            step
        IO.println s!"{result}"
        IO.println s!"{machine'.reg_file.data}"
