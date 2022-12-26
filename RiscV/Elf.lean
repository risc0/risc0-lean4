/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import Elf
import RiscV.Config
import RiscV.Mach.Mem
import RiscV.Mach.Reg
import RiscV.Monad

namespace RiscV.Elf

open R0sy.Lean.ByteArray
open Elf
open RiscV.Config
open RiscV.Mach.Mem
open RiscV.Mach.Reg
open RiscV.Monad

def xlenOfElf (elf: Elf): Xlen
  := match elf.e_header.e_ident.ei_class with
      | .Ptr32 => .Xlen32
      | .Ptr64 => .Xlen64

def endianOfElf (elf: Elf): Endian
  := match elf.e_header.e_ident.ei_data with
      | .Big => .Big
      | .Little => .Little

def loadElf (elf: Elf): MachineState
  := Id.run do
        let mut blocks: Array Block := Array.mkEmpty 0
        for segment in elf.programs do
          if segment.header.p_type == .PT_LOAD
            then do let zerosRequired := segment.header.p_memsz.toNat - segment.header.p_filesz.toNat
                    let zeros: Array UInt8 := Array.mkArray zerosRequired 0
                    let data := segment.file_data.toArray ++ zeros
                    blocks := blocks.push {
                      base := segment.header.p_vaddr.toNat,
                      data
                    }
        pure {
          reg_file := RegFile.newWithPc elf.e_header.e_entry.toNat.toUInt32
          mem := { endian := endianOfElf elf, blocks }
        }

def loadElfInfo (mach: MachineState) (elf: Elf): Except Mach.Exception.RiscVException MachineState
  := Id.run do
        mach.run'
          <| do let entry := elf.e_header.e_entry.toNat.toUInt32
                RegFile.set_word .PC entry
                for segment in elf.programs do
                  if segment.header.p_type == .PT_LOAD
                    then do let base := segment.header.p_vaddr.toNat
                            for i in [0:segment.file_data.size] do
                              Mem.set_byte (base + i) segment.file_data[i]!
                MonadMachine.getMachine

end RiscV.Elf
