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
        IO.println s!"Running ..."
        let isa := RiscV.ISA.RV32IM.ISA
        let debug: Bool := false
        let maxClock := 16 <<< 20
        let (result, machine) <- initialMachine.run do
          for clock in [0:maxClock] do
            if debug
              then do let machine <- MonadMachine.getMachine
                      let pc <- MonadMachine.getReg .PC
                      let instr <- tryCatch (MonadMachine.fetchWord pc) (fun _ => pure 0)
                      monadLift <| IO.println s!"Register File: {machine.reg_file}"
                      monadLift <| IO.println s!"Next instr: {R0sy.Data.Hex.UInt32.toHex instr}  {isa.decode_to_string instr}"
                      monadLift <| IO.println s!""
            if clock % (64 <<< 10) == 0 then IO.println s!"... clock {clock}"
            /- Run the next instruction -/
            let result <-
              tryCatch (isa.step >>= fun _ => pure none)
                <| fun exception
                    => do IO.println s!"Exception at clock: {clock}"
                          pure (some exception)
            match result with
              | none => pure ()
              | some exception => throw exception
        match result with
          | Except.ok _ => IO.println s!"Ended normally after {maxClock} clock cycles"
          | Except.error exception => IO.println s!"Ended with exception: {exception}"
        IO.println s!"Final register file: {machine.reg_file}"
