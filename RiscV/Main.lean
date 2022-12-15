/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV

def fib: Array UInt32 := #[
  0x00500993,   /- ADDI  rd:19  rs1:0   imm:5   -/
  0x00000a13,   /- ADDI  rd:20  rs1:0   imm:0   -/
  0x00100913,   /- ADDI  rd:18  rs1:0   imm:1   -/
  0x00000493,   /- ADDI  rd:9   rs1:0   imm:0   -/
  0x0140006f,   /- JAL   rd:0           imm:20  -/
  0x012a0ab3,   /- ADD   rd:21  rs1:20  rs2:18  -/
  0x00090a13,   /- ADDI  rd:20  rs1:18  imm:0   -/
  0x000a8913,   /- ADDI  rd:18  rs1:21  imm:0   -/
  0x00148493,   /- ADDI  rd:9   rs1:9   imm:1   -/
  0xff34c8e3    /- BLT   rs1:9  rs2:19  imm:-16 -/
]

def main : IO Unit
  := do let mut machine: RiscV.Monad.Machine := {
          reg_file := RiscV.Mach.Reg.RegFile.new
          mem := {
            blocks := #[
              {
                base := { val := 0x00000000 },
                data := fib
              }
            ]
          }
        }
        let step {M: Type -> Type} [RiscV.Monad.MonadMachine M]: M Unit := RiscV.Instr.Sets.InstructionSet.step RiscV.Instr.RV32IM.RV32IM
        let decode_to_string := RiscV.Instr.Sets.InstructionSet.decode_to_string RiscV.Instr.RV32IM.RV32IM
        for _ in [0:50] do
          let result <- RiscV.Monad.MonadMachine.run' machine do
            let pc <- RiscV.Mach.Reg.RegFile.get_word .PC
            let instr <- tryCatch (RiscV.Mach.Mem.Mem.get_word { val := pc }) (fun _ => pure 0)
            pure (pc, decode_to_string instr)
          match result with
            | Except.ok (pc, some instr)
                => do IO.println s!"Register File: {machine.reg_file}"
                      IO.println s!"Next:  pc:{pc}  instr:{instr}"
            | _ => pure ()
          let result <- RiscV.Monad.MonadMachine.run machine step
          match result with
            | Except.ok ((), machine')
                => do machine := machine'
                      IO.println ""
            | Except.error exception
                => do IO.println s!"{exception}"
                      return ()
        IO.println "done!"
