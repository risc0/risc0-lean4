/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Elf.Types

namespace Elf.Section

open R0sy.ByteDeserial
open Elf.Types

structure SHeader (ptrSize: PtrSize) where
  sh_name: UInt32
  sh_type: UInt32
  sh_flags: Ptr ptrSize
  sh_addr: Ptr ptrSize
  sh_offset: Ptr ptrSize
  sh_size: Ptr ptrSize
  sh_link: UInt32
  sh_info: UInt32
  sh_addralign: Ptr ptrSize
  sh_entsize: Ptr ptrSize

namespace SHeader
  def parse [Monad M] [MonadByteReader M] (ptrSize: PtrSize) (endianness: Endianness): M (SHeader ptrSize)
    := do let sh_name <- parseUInt32 endianness
          let sh_type <- parseUInt32 endianness
          let sh_flags <- parsePtr ptrSize endianness
          let sh_addr <- parsePtr ptrSize endianness
          let sh_offset <- parsePtr ptrSize endianness
          let sh_size <- parsePtr ptrSize endianness
          let sh_link <- parseUInt32 endianness
          let sh_info <- parseUInt32 endianness
          let sh_addralign <- parsePtr ptrSize endianness
          let sh_entsize <- parsePtr ptrSize endianness
          pure {
            sh_name,
            sh_type,
            sh_flags,
            sh_addr,
            sh_offset,
            sh_size,
            sh_link,
            sh_info,
            sh_addralign,
            sh_entsize
          }
end SHeader

structure Section (ptrSize: PtrSize) where
  header: SHeader ptrSize
  file_data: Subarray UInt8

namespace Section
  def parse [Monad M] [MonadByteReader M] (ptrSize: PtrSize) (endianness: Endianness): M (Section ptrSize)
    := do let header <- SHeader.parse ptrSize endianness
          let start := header.sh_offset.toNat
          let stop := start + header.sh_size.toNat
          let file_data <- MonadByteReader.getSubarray start stop
          pure {
            header,
            file_data
          }
end Section

end Elf.Section
