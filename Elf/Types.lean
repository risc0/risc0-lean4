/-
Copyright (c) 2022 RISC Zero. All rights reserved.
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
