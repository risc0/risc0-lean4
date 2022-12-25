/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV
import Zkvm

open RiscV.Monad

def main : IO Unit
  := do let filename := "rust/output/hw.bin"
        IO.println s!"Loading {filename}"
        let result <- Elf.ofFile filename
        let elf
          <- match result with
              | Except.ok elf => pure elf
              | Except.error error
                  => do IO.println s!"Error: {error}"
                        return ()
        IO.println s!"Creating initial machine state"
        let initialMachine
          <- match Zkvm.Platform.Elf.loadElf elf with
              | Except.ok mach => pure mach
              | Except.error exception
                  => do IO.println s!"Error: {exception}"
                        return ()
        for block in initialMachine.mem.blocks do
          IO.println s!"Memory block: {R0sy.Data.Hex.UInt32.toHex block.base.toUInt32} - {R0sy.Data.Hex.UInt32.toHex block.limit.toUInt32}"
        let isa := RiscV.ISA.RV32IM.ISA
        let debug: Bool := false
        let maxClock := 1024
        let (result, machine) <- initialMachine.run do
          for clock in [0:maxClock] do
            if debug
              then do let machine <- MonadMachine.getMachine
                      let pc <- MonadMachine.getReg .PC
                      let instr <- tryCatch (MonadMachine.fetchWord pc) (fun _ => pure 0)
                      monadLift <| IO.println s!"Register File: {machine.reg_file}"
                      monadLift <| IO.println s!"Next instr: {R0sy.Data.Hex.UInt32.toHex instr}  {isa.decode_to_string instr}"
                      monadLift <| IO.println s!""
            if clock % 32 == 0 then IO.println s!"... clock {clock}"
            /- Run the next instruction -/
            isa.step
        match result with
          | Except.ok _ => IO.println s!"Ended normally after {maxClock} clock cycles"
          | Except.error exception => IO.println s!"Ended with exception: {exception}"
        IO.println s!"Final register file: {machine.reg_file}"
