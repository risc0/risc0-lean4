/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV

open RiscV.Monad

def main : IO Unit
  := do let filename := "rust/output/hw.bin"
        let result <- Elf.ofFile filename
        let elf
          <- match result with
              | Except.ok elf => pure elf
              | Except.error error
                  => do IO.println s!"ERROR: {error}"
                        return ()
        let initialMachine := RiscV.Elf.loadElf elf
        for block in initialMachine.mem.blocks do
          IO.println s!"Memory block: {R0sy.Data.Hex.UInt32.toHex block.base.toUInt32} - {R0sy.Data.Hex.UInt32.toHex block.end.toUInt32}"
        let (result, machine) <- initialMachine.run do
          let variant <- MonadMachine.getVariant
          for _ in [0:50] do
            /- Print some info about the machine state -/
            let machine <- MonadMachine.getMachine
            let pc <- MonadMachine.getReg .PC
            let instr <- tryCatch (MonadMachine.fetchWord pc) (fun _ => pure 0)
            monadLift <| IO.println s!"Register File: {machine.reg_file}"
            monadLift <| IO.println s!"Next instr: {R0sy.Data.Hex.UInt32.toHex instr}  {variant.isa.decode_to_string instr}"
            monadLift <| IO.println s!""
            /- Run the next instruction -/
            MonadMachine.step
        match result with
          | Except.ok _ => IO.println "Ended normally"
          | Except.error exception => IO.println s!"Ended with exception: {exception}"
        IO.println s!"Final register file: {machine.reg_file}"
