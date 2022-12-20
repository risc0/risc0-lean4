/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Elf.Types

namespace Elf.Program

open R0sy.ByteDeserial
open Elf.Types

structure PHeader (ptrSize: PtrSize) (endianness: Endianness) where
  p_type: UInt32
  p_flags: UInt32
  p_offset: Ptr ptrSize
  p_vaddr: Ptr ptrSize
  p_paddr: Ptr ptrSize
  p_filesz: Ptr ptrSize
  p_memsz: Ptr ptrSize
  p_align: Ptr ptrSize

namespace PHeader
  def parse [Monad M] [MonadByteReader M] (ptrSize: PtrSize) (endianness: Endianness): M (PHeader ptrSize endianness)
    := do let mut p_flags := 0
          let p_type <- parseUInt32 endianness
          if ptrSize == .Ptr64 then p_flags <- parseUInt32 endianness
          let p_offset <- parsePtr ptrSize endianness
          let p_vaddr <- parsePtr ptrSize endianness
          let p_paddr <- parsePtr ptrSize endianness
          let p_filesz <- parsePtr ptrSize endianness
          let p_memsz <- parsePtr ptrSize endianness
          if ptrSize == .Ptr32 then p_flags <- parseUInt32 endianness
          let p_align <- parsePtr ptrSize endianness
          pure {
            p_type,
            p_flags,
            p_offset,
            p_vaddr,
            p_paddr,
            p_filesz,
            p_memsz,
            p_align
          }
end PHeader

structure Program (ptrSize: PtrSize) (endianness: Endianness) where
  header: PHeader ptrSize endianness
  file_data: Subarray UInt8

namespace Program
  def parse [Monad M] [MonadByteReader M] (ptrSize: PtrSize) (endianness: Endianness): M (Program ptrSize endianness)
    := do let header <- PHeader.parse ptrSize endianness
          let start := header.p_offset.toNat
          let stop := start + header.p_filesz.toNat
          let file_data <- MonadByteReader.getSubarray start stop
          pure {
            header,
            file_data
          }
end Program

end Elf.Program
