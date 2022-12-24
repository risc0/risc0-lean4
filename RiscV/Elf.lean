/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import Elf
import RiscV.Mach.Mem
import RiscV.Mach.Reg
import RiscV.Monad

namespace RiscV.Elf

open R0sy.Lean.ByteArray
open Elf
open RiscV.Mach.Mem
open RiscV.Mach.Reg
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

def loadElfInfo (mach: Machine .RV32IMle) (elf: Elf): Machine .RV32IMle
  := Id.run do
        let mach' := loadElf elf
        {
          reg_file := mach'.reg_file,
          mem := {
            blocks := mach'.mem.blocks ++ mach.mem.blocks
          }
        }

-- def loadElfInfo (mach: Machine .RV32IMle) (elf: Elf): Machine .RV32IMle
--   := Id.run do
--         let (_, mach')
--           <- Machine.run mach
--               <| do RegFile.set_word .PC elf.e_header.e_entry.toNat.toUInt32
--                     for segment in elf.programs do
--                       if segment.header.p_type == .PT_LOAD then do
--                         let base := segment.header.p_vaddr.toNat
--                         for i in [0:segment.file_data.size] do
--                           Mem.set_byte (base + i) segment.file_data[i]!
--         mach'

end RiscV.Elf
