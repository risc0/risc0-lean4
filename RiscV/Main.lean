/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV

open RiscV.Monad

def fib: Machine .RV32IMle
  := Machine.new_RV32IMle
      0x00000000
      {
        blocks := #[
          {
            base := { val := 0x00000000 },
            data := #[
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
          }
        ]
      }

def main : IO Unit
  := do let (result, machine) <- fib.run do
          let variant <- MonadMachine.getVariant
          for _ in [0:50] do
            /- Print some info about the machine state -/
            let machine <- MonadMachine.getMachine
            let pc <- MonadMachine.getReg .PC
            let instr <- tryCatch (MonadMachine.fetchWord pc) (fun _ => pure 0)
            monadLift <| IO.println s!"Register File: {machine.reg_file}"
            monadLift <| IO.println s!"Next:  pc:{pc}  instr:{variant.isa.decode_to_string instr}"
            monadLift <| IO.println s!""
            /- Run the next instruction -/
            MonadMachine.step
        match result with
          | Except.ok _ => IO.println "Ended normally"
          | Except.error exception => IO.println s!"Ended with exception: {exception}"
        IO.println s!"Final register file: {machine.reg_file}"
