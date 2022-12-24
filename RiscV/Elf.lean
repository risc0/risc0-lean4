/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import Elf
import RiscV.Mach.Mem
import RiscV.Monad

namespace RiscV.Elf

open R0sy.Lean.ByteArray
open Elf
open RiscV.Mach.Mem
open RiscV.Monad

def loadElf (elf: Elf): Machine .RV32IMle
  := Id.run do
        let mut blocks: Array Block := Array.mkEmpty 0
        for segment in elf.programs do
          if segment.header.p_type == .PT_LOAD then do
            let zerosRequired := segment.header.p_memsz.toNat - segment.header.p_filesz.toNat
            let zeros: Array UInt8 := Array.mkArray zerosRequired 0
            let data := segment.file_data.toArray ++ zeros
            blocks := blocks.push {
              base := segment.header.p_vaddr.toNat,
              data
            }
        let pc := elf.e_header.e_entry.toNat.toUInt32
        let mem: Mem := { blocks }
        pure (Machine.new_RV32IMle pc mem)

end RiscV.Elf
