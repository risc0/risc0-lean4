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

import R0sy.Lean.Subarray

namespace R0sy.ByteDeserial

open Lean.Subarray

/- Monad abstraction -/

inductive ByteReaderError where
  | InvalidData
  | OutOfData

instance : ToString ByteReaderError where
  toString error
    := match error with
        | ByteReaderError.InvalidData => "InvalidData"
        | ByteReaderError.OutOfData => "OutOfData"

class MonadByteReader (M: Type -> Type)
    extends MonadExcept ByteReaderError M
  where
    readBytes: Nat -> M (Subarray UInt8)
    onSubarray: (start stop: Nat) -> (f: M X) -> M X
    getSubarray: (start stop: Nat) -> M (Subarray UInt8)

def MonadByteReader.readUInt8 [Monad M] [MonadByteReader M]: M UInt8
  := do let result <- MonadByteReader.readBytes 1
        let c0 := result[0]!
        pure c0

def MonadByteReader.readUInt16le [Monad M] [MonadByteReader M]: M UInt16
  := do let result <- MonadByteReader.readBytes 2
        let c0 := result[0]!.toNat
        let c1 := result[1]!.toNat
        let d := c0 ||| (c1 <<< 8)
        pure (UInt16.ofNat d)

def MonadByteReader.readUInt16be [Monad M] [MonadByteReader M]: M UInt16
  := do let result <- MonadByteReader.readBytes 2
        let c0 := result[0]!.toNat
        let c1 := result[1]!.toNat
        let d := c1 ||| (c0 <<< 8)
        pure (UInt16.ofNat d)

def MonadByteReader.readUInt32le [Monad M] [MonadByteReader M]: M UInt32
  := do let result <- MonadByteReader.readBytes 4
        let c0 := result[0]!.toNat
        let c1 := result[1]!.toNat
        let c2 := result[2]!.toNat
        let c3 := result[3]!.toNat
        let d := c0 ||| (c1 <<< 8) ||| (c2 <<< 16) ||| (c3 <<< 24)
        pure (UInt32.ofNat d)

def MonadByteReader.readUInt32be [Monad M] [MonadByteReader M]: M UInt32
  := do let result <- MonadByteReader.readBytes 4
        let c0 := result[0]!.toNat
        let c1 := result[1]!.toNat
        let c2 := result[2]!.toNat
        let c3 := result[3]!.toNat
        let d := c3 ||| (c2 <<< 8) ||| (c1 <<< 16) ||| (c0 <<< 24)
        pure (UInt32.ofNat d)

def MonadByteReader.readUInt64le [Monad M] [MonadByteReader M]: M UInt64
  := do let result <- MonadByteReader.readBytes 8
        let c0 := result[0]!.toNat
        let c1 := result[1]!.toNat
        let c2 := result[2]!.toNat
        let c3 := result[3]!.toNat
        let c4 := result[4]!.toNat
        let c5 := result[5]!.toNat
        let c6 := result[6]!.toNat
        let c7 := result[7]!.toNat
        let d := c0 ||| (c1 <<< 8) ||| (c2 <<< 16) ||| (c3 <<< 24) ||| (c4 <<< 32) ||| (c5 <<< 40) ||| (c6 <<< 44) ||| (c7 <<< 52)
        pure (UInt64.ofNat d)

def MonadByteReader.readUInt64be [Monad M] [MonadByteReader M]: M UInt64
  := do let result <- MonadByteReader.readBytes 8
        let c0 := result[0]!.toNat
        let c1 := result[1]!.toNat
        let c2 := result[2]!.toNat
        let c3 := result[3]!.toNat
        let c4 := result[4]!.toNat
        let c5 := result[5]!.toNat
        let c6 := result[6]!.toNat
        let c7 := result[7]!.toNat
        let d := c7 ||| (c6 <<< 8) ||| (c5 <<< 16) ||| (c4 <<< 24) ||| (c3 <<< 32) ||| (c2 <<< 40) ||| (c1 <<< 44) ||| (c0 <<< 52)
        pure (UInt64.ofNat d)

def MonadByteReader.readMany [Monad M] [MonadByteReader M] (num: Nat) (readX: M X) (out: Array X := Array.mkEmpty num): M (Array X)
  := match num with
      | 0 => pure out
      | num + 1
          => do let x <- readX
                MonadByteReader.readMany num readX (out.push x)

def MonadByteReader.readArray [Monad M] [MonadByteReader M] (readX: M X): M (Array X)
  := do let size <- MonadByteReader.readUInt32le
        MonadByteReader.readMany size.toNat readX

def MonadByteReader.readUInt16le_array [Monad M] [MonadByteReader M]: M (Array UInt16)
  := MonadByteReader.readArray MonadByteReader.readUInt16le

def MonadByteReader.readUInt16be_array [Monad M] [MonadByteReader M]: M (Array UInt16)
  := MonadByteReader.readArray MonadByteReader.readUInt16be

def MonadByteReader.readUInt32le_array [Monad M] [MonadByteReader M]: M (Array UInt32)
  := MonadByteReader.readArray MonadByteReader.readUInt32le

def MonadByteReader.readUInt32be_array [Monad M] [MonadByteReader M]: M (Array UInt32)
  := MonadByteReader.readArray MonadByteReader.readUInt32be

def MonadByteReader.readUInt64le_array [Monad M] [MonadByteReader M]: M (Array UInt64)
  := MonadByteReader.readArray MonadByteReader.readUInt64le

def MonadByteReader.readUInt64be_array [Monad M] [MonadByteReader M]: M (Array UInt64)
  := MonadByteReader.readArray MonadByteReader.readUInt64be


/- Basic instance -/

structure ByteReader where
  data: Subarray UInt8

def ByteReader.readBytes [Monad M] [MonadStateOf ByteReader M] [MonadExcept ByteReaderError M] (bytes: Nat): M (Subarray UInt8)
  := do let self <- get
        if self.data.size < bytes then throw ByteReaderError.OutOfData
        let (result, data) := Subarray.take self.data bytes
        set { self with data }
        pure result

instance [Monad M] [MonadStateOf ByteReader M] [MonadExcept ByteReaderError M] : MonadByteReader M where
  readBytes := ByteReader.readBytes
  onSubarray {X} (start stop: Nat) (f: M X)
    := do let self <- get
          let sub: ByteReader := { data := self.data.as.toSubarray start stop }
          set sub
          let x <- f
          set self
          pure x
  getSubarray (start stop: Nat)
    := do let self <- get
          pure (self.data.as.toSubarray start stop)

def ByteReader.run
  (f: {M: Type -> Type} -> [Monad M] -> [MonadByteReader M] -> M X)
  (buffer: Subarray UInt8)
  : Except ByteReaderError X
  :=
  let M: Type -> Type := StateT ByteReader (ExceptT ByteReaderError Id)
  ExceptT.run (StateT.run' (@f M _ _) { data := buffer })

end R0sy.ByteDeserial
