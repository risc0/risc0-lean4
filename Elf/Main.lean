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

import R0sy
import Elf

def dump_elf (elf: Elf): IO Unit
  := do IO.println s!"PC: {elf.e_header.e_entry.toNat}"
        IO.println s!"Program headers: {elf.programs.size}"
        for program in elf.programs do
          IO.println s!"  vaddr: {program.header.p_vaddr.toNat} paddr: {program.header.p_paddr.toNat} filesz: {program.header.p_filesz.toNat} memsz: {program.header.p_memsz.toNat}"
        IO.println s!"Section headers: {elf.sections.size}"
        for sec in elf.sections do
          IO.println s!"  addr: {sec.header.sh_addr.toNat} size: {sec.header.sh_size.toNat}"

def main : IO Unit
  := do let filename := "rust/output/hw.bin"
        let result <- Elf.ofFile filename
        match result with
        | Except.ok elf => dump_elf elf
        | Except.error error => IO.println s!"ERROR: {error}"
