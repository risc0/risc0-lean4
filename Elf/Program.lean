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
import Elf.Types

namespace Elf.Program

open R0sy.ByteDeserial
open Elf.Types

inductive SegmentType where
  | PT_NULL
  | PT_LOAD
  | PT_DYNAMIC
  | PT_INTERP
  | PT_NOTE
  | PT_SHLIB
  | PT_PHDR
  | PT_TLS
  | PT_RESERVED_OS (p_type: UInt32)
  | PT_RESERVED_PROC (p_type: UInt32)
  deriving BEq


def SegmentType.ofUInt32 [Monad M] [MonadByteReader M] (val: UInt32): M SegmentType
  := match val with
      | 0x00000000 => pure PT_NULL
      | 0x00000001 => pure PT_LOAD
      | 0x00000002 => pure PT_DYNAMIC
      | 0x00000003 => pure PT_INTERP
      | 0x00000004 => pure PT_NOTE
      | 0x00000005 => pure PT_SHLIB
      | 0x00000006 => pure PT_PHDR
      | 0x00000007 => pure PT_TLS
      | _ =>
          if 0x60000000 <= val && val <= 0x6FFFFFFF then pure (.PT_RESERVED_OS val)
          else if 0x70000000 <= val && val <= 0x7FFFFFFF then pure (.PT_RESERVED_PROC val)
          else do panic! s!"p_type: {val}"
                  throw .InvalidData

structure PHeader (ptrSize: PtrSize) where
  p_type: SegmentType
  p_flags: UInt32
  p_offset: Ptr ptrSize
  p_vaddr: Ptr ptrSize
  p_paddr: Ptr ptrSize
  p_filesz: Ptr ptrSize
  p_memsz: Ptr ptrSize
  p_align: Ptr ptrSize

namespace PHeader
  def parse [Monad M] [MonadByteReader M] (ptrSize: PtrSize) (endianness: Endianness): M (PHeader ptrSize)
    := do let mut p_flags := 0
          let p_type <- parseUInt32 endianness >>= SegmentType.ofUInt32
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

structure Program (ptrSize: PtrSize) where
  header: PHeader ptrSize
  file_data: Subarray UInt8

namespace Program
  def parse [Monad M] [MonadByteReader M] (ptrSize: PtrSize) (endianness: Endianness): M (Program ptrSize)
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
