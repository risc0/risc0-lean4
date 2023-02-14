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

namespace Elf.Header

open R0sy.ByteDeserial
open Elf.Types

structure EIdent where
  ei_class: PtrSize
  ei_data: Endianness
  ei_version: UInt8
  ei_osabi: UInt8
  ei_abiversion: UInt8

namespace EIdent
  def parse [Monad M] [MonadByteReader M]: M EIdent
    := do if (<- MonadByteReader.readUInt8) != 0x7f then throw .InvalidData
          if (<- MonadByteReader.readUInt8) != 0x45 then throw .InvalidData
          if (<- MonadByteReader.readUInt8) != 0x4c then throw .InvalidData
          if (<- MonadByteReader.readUInt8) != 0x46 then throw .InvalidData
          let ei_class_raw <- MonadByteReader.readUInt8
          let ei_class <-
            match ei_class_raw with
            | 1 => pure .Ptr32
            | 2 => pure .Ptr64
            | _ => do panic! s!"ei_class_raw: {ei_class_raw}"
                      throw .InvalidData
          let ei_data_raw <- MonadByteReader.readUInt8
          let ei_data <-
            match ei_data_raw with
            | 1 => pure .Little
            | 2 => pure .Big
            | _ => do panic! s!"ei_data_raw: {ei_data_raw}"
                      throw .InvalidData
          let ei_version <- MonadByteReader.readUInt8
          let ei_osabi <- MonadByteReader.readUInt8
          let ei_abiversion <- MonadByteReader.readUInt8
          for _ in [0:7] do
            let _ <- MonadByteReader.readUInt8
          pure {
            ei_class,
            ei_data,
            ei_version,
            ei_osabi,
            ei_abiversion,
          }
end EIdent

structure Header where
  e_ident: EIdent
  e_type: UInt16
  e_machine: UInt16
  e_version: UInt32
  e_entry: Ptr e_ident.ei_class
  e_phoff: Ptr e_ident.ei_class
  e_shoff: Ptr e_ident.ei_class
  e_flags: UInt32
  e_ehsize: UInt16
  e_phentsize: UInt16
  e_phnum: UInt16
  e_shentsize: UInt16
  e_shnum: UInt16
  e_shstrndx: UInt16

namespace Header
  def parse [Monad M] [MonadByteReader M]: M Header
    := do let e_ident <- EIdent.parse
          let ptrSize := e_ident.ei_class
          let endianness := e_ident.ei_data
          let e_type <- parseUInt16 endianness
          let e_machine <- parseUInt16 endianness
          let e_version <- parseUInt32 endianness
          let e_entry <- parsePtr ptrSize endianness
          let e_phoff <- parsePtr ptrSize endianness
          let e_shoff <- parsePtr ptrSize endianness
          let e_flags <- parseUInt32 endianness
          let e_ehsize <- parseUInt16 endianness
          let e_phentsize <- parseUInt16 endianness
          let e_phnum <- parseUInt16 endianness
          let e_shentsize <- parseUInt16 endianness
          let e_shnum <- parseUInt16 endianness
          let e_shstrndx <- parseUInt16 endianness
          pure {
            e_ident,
            e_type,
            e_machine,
            e_version,
            e_entry,
            e_phoff,
            e_shoff,
            e_flags,
            e_ehsize,
            e_phentsize,
            e_phnum,
            e_shentsize,
            e_shnum,
            e_shstrndx
          }
end Header

end Elf.Header
