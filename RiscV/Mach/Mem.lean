/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV.Config
import RiscV.Mach.Exception
import RiscV.Mach.XlenInt

namespace RiscV.Mach.Mem

open RiscV.Config
open RiscV.Mach.Exception
open RiscV.Mach.XlenInt

structure Block where
  base: Nat
  data: Array UInt8
  deriving Inhabited

namespace Block
  def size_in_bytes (self: Block): Nat := self.data.size

  def limit (self: Block): Nat
    := self.base + self.size_in_bytes

  def contains (self: Block) (addr: Nat): Bool
    := self.base <= addr && addr < self.limit

  def get_byte (self: Block) (addr: Nat): UInt8
    := Array.getD self.data (addr - self.base) 0

  @[always_inline, inline]
  def set_byte (self: Block) (addr: Nat) (val: UInt8): Block
    := {
      self with
      data := Array.setD self.data (addr - self.base) val
    }
end Block


structure Mem where
  endian: Endian
  blocks: Array Block

namespace Mem
  def getEndian [Monad M] [MonadStateOf Mem M]: M Endian
    := do let self <- get
          pure self.endian

  def locate_block [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (addr: Nat): M Nat
    := do let self <- get
          for i in [0:self.blocks.size] do
            if self.blocks[i]!.contains addr then return i
          throw (RiscVException.PtrOutOfBounds addr)

  def get_byte [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (addr: Nat): M UInt8
    := do let self <- get
          let idx <- Mem.locate_block addr
          pure <| self.blocks[idx]!.get_byte addr

  @[always_inline, inline]
  def set_byte [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (addr: Nat) (val: UInt8): M Unit
    := do let mut self <- get
          let idx <- Mem.locate_block addr
          let block := self.blocks[idx]!
          set {
            self with
            blocks := Array.setD self.blocks idx Inhabited.default
          }
          self <- get
          set {
            self with
            blocks := Array.setD self.blocks idx (block.set_byte addr val)
          }

  def get_half [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (addr: Nat): M UInt16
    := do let x0 := (<- Mem.get_byte addr).toUInt16
          let x1 := (<- Mem.get_byte (addr + 1)).toUInt16
          pure <| match (<- getEndian) with
                  | .Big => (x0 <<< 8) ||| x1
                  | .Little => (x1 <<< 8) ||| x0

  def set_half [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (addr: Nat) (val: UInt16): M Unit
    := do let lo := val.toNat.toUInt8
          let hi := (val >>> 8).toNat.toUInt8
          let (x0, x1)
            := match (<- getEndian) with
                | .Big => (hi, lo)
                | .Little => (lo, hi)
          Mem.set_byte addr x0
          Mem.set_byte (addr + 1) x1

  def get_word [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (addr: Nat): M UInt32
    := do let x0 := (<- Mem.get_half addr).toUInt32
          let x1 := (<- Mem.get_half (addr + 2)).toUInt32
          pure <| match (<- getEndian) with
                  | .Big => (x0 <<< 16) ||| x1
                  | .Little => (x1 <<< 16) ||| x0

  def set_word [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (addr: Nat) (val: UInt32): M Unit
    := do let lo := val.toNat.toUInt16
          let hi := (val >>> 16).toNat.toUInt16
          let (x0, x1)
            := match (<- getEndian) with
                | .Big => (hi, lo)
                | .Little => (lo, hi)
          Mem.set_half addr x0
          Mem.set_half (addr + 2) x1

  def get_wide [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (addr: Nat): M UInt64
    := do let x0 := (<- Mem.get_word addr).toUInt64
          let x1 := (<- Mem.get_word (addr + 4)).toUInt64
          pure <| match (<- getEndian) with
                  | .Big => (x0 <<< 32) ||| x1
                  | .Little => (x1 <<< 32) ||| x0

  def set_wide [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (addr: Nat) (val: UInt64): M Unit
    := do let lo := val.toNat.toUInt32
          let hi := (val >>> 32).toNat.toUInt32
          let (x0, x1)
            := match (<- getEndian) with
                | .Big => (hi, lo)
                | .Little => (lo, hi)
          Mem.set_word addr x0
          Mem.set_word (addr + 4) x1
end Mem

end RiscV.Mach.Mem
