/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Elf.Header
import Elf.Section
import Elf.Program
import Elf.Types

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

