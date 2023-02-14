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

namespace Elf.Types

open R0sy.ByteDeserial

inductive PtrSize where
  | Ptr32
  | Ptr64
  deriving BEq

inductive Endianness where
  | Big
  | Little
  deriving BEq

def Ptr: PtrSize -> Type
  | .Ptr32 => UInt32
  | .Ptr64 => UInt64

def Ptr.toNat: {ptrSize: PtrSize} -> Ptr ptrSize -> Nat
  | .Ptr32, (val: UInt32) => val.toNat
  | .Ptr64, (val: UInt64) => val.toNat

def parseUInt16 [Monad M] [MonadByteReader M]: Endianness -> M UInt16
  | .Big => MonadByteReader.readUInt16be
  | .Little => MonadByteReader.readUInt16le

def parseUInt32 [Monad M] [MonadByteReader M]: Endianness -> M UInt32
  | .Big => MonadByteReader.readUInt32be
  | .Little => MonadByteReader.readUInt32le

def parseUInt64 [Monad M] [MonadByteReader M]: Endianness -> M UInt64
  | .Big => MonadByteReader.readUInt64be
  | .Little => MonadByteReader.readUInt64le

def parsePtr [Monad M] [MonadByteReader M]: (ptrSize: PtrSize) -> Endianness -> M (Ptr ptrSize)
  | .Ptr32 => parseUInt32
  | .Ptr64 => parseUInt64

end Elf.Types
