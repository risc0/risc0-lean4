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
import Elf.Header
import Elf.Section
import Elf.Program
import Elf.Types

/-!
# Executable and Linkable Format

The Executable and Linkable Format (ELF).
-/


structure Elf where
  e_header: Elf.Header.Header
  programs: Array (Elf.Program.Program e_header.e_ident.ei_class)
  sections: Array (Elf.Section.Section e_header.e_ident.ei_class)

namespace Elf
  open R0sy.ByteDeserial
  open Elf.Header
  open Elf.Section
  open Elf.Program
  open Elf.Types

  def parse [Monad M] [MonadByteReader M]: M Elf
    := do let e_header <- Header.parse
          let ptrSize := e_header.e_ident.ei_class
          let endianness := e_header.e_ident.ei_data
          let programs
            <- do let num_programs := e_header.e_phnum.toNat
                  let mut programs := Array.mkEmpty num_programs
                  for i in [0:num_programs] do
                    let start := e_header.e_phoff.toNat + i * e_header.e_phentsize.toNat
                    let stop := start + e_header.e_phentsize.toNat
                    let program <- MonadByteReader.onSubarray start stop (Program.parse ptrSize endianness)
                    programs := programs.push program
                  pure programs
          let sections
            <- do let num_sections := e_header.e_shnum.toNat
                  let mut sections := Array.mkEmpty num_sections
                  for i in [0:num_sections] do
                    let start := e_header.e_shoff.toNat + i * e_header.e_shentsize.toNat
                    let stop := start + e_header.e_shentsize.toNat
                    let sec <- MonadByteReader.onSubarray start stop (Section.parse ptrSize endianness)
                    sections := sections.push sec
                  pure sections
          pure {
            e_header,
            programs,
            sections
          }

  def ofFile (filename: System.FilePath): IO (Except ByteReaderError Elf)
    := do let meta <- filename.metadata
          let byteSize := meta.byteSize
          let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
          let bin <- handle.read (byteSize.toNat.toUSize)
          pure <| ByteReader.run Elf.parse bin.data.toSubarray
end Elf

