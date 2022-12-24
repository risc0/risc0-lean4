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

def Block.size_in_bytes (self: Block): Nat := self.data.size

def Block.end (self: Block): Nat
  := self.base + self.size_in_bytes

def Block.contains (self: Block) (addr: Nat): Bool
  := self.base <= addr && addr < self.end

def Block.get_byte (self: Block) (addr: Nat): UInt8
  := Array.getD self.data (addr - self.base) 0

def Block.set_byte (self: Block) (addr: Nat) (val: UInt8): Block
  := {
    self with
    data := Array.setD self.data (addr - self.base) val
  }


structure Mem where
  blocks: Array Block
  deriving Inhabited

def Mem.locate_block [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (addr: Nat): M Nat
  := do let self <- get
        for i in [0:self.blocks.size] do
          if self.blocks[i]!.contains addr then return i
        throw (RiscVException.PtrOutOfBounds addr)

def Mem.get_byte [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (addr: Nat): M UInt8
  := do let self <- get
        let idx <- Mem.locate_block addr
        pure <| self.blocks[idx]!.get_byte addr

def Mem.set_byte [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (addr: Nat) (val: UInt8): M Unit
  := do let self <- get
        let idx <- Mem.locate_block addr
        set {
          self with
          blocks := Array.setD self.blocks idx (self.blocks[idx]!.set_byte addr val)
        }

def Mem.get_half [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (endian: Endian) (addr: Nat): M UInt16
  := do let (lo_off, hi_off)
          := match endian with
              | .Big => (1, 0)
              | .Little => (0, 1)
        let lo := (<- Mem.get_byte (addr + lo_off)).toUInt16
        let hi := (<- Mem.get_byte (addr + hi_off)).toUInt16
        pure ((hi <<< 8) ||| lo)

def Mem.set_half [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (endian: Endian) (addr: Nat) (val: UInt16): M Unit
  := do let (lo_off, hi_off)
          := match endian with
              | .Big => (1, 0)
              | .Little => (0, 1)
        let lo := (val &&& 0xff).toNat.toUInt8
        let hi := ((val >>> 8) &&& 0xff).toNat.toUInt8
        Mem.set_byte (addr + lo_off) lo
        Mem.set_byte (addr + hi_off) hi

def Mem.get_word [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (endian: Endian) (addr: Nat): M UInt32
  := do let (lo_off, hi_off)
          := match endian with
              | .Big => (2, 0)
              | .Little => (0, 2)
        let lo := (<- Mem.get_half endian (addr + lo_off)).toUInt32
        let hi := (<- Mem.get_half endian (addr + hi_off)).toUInt32
        pure ((hi <<< 16) ||| lo)

def Mem.set_word [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (endian: Endian) (addr: Nat) (val: UInt32): M Unit
  := do let (lo_off, hi_off)
          := match endian with
              | .Big => (2, 0)
              | .Little => (0, 2)
        let lo := (val &&& 0xffff).toNat.toUInt16
        let hi := ((val >>> 16) &&& 0xffff).toNat.toUInt16
        Mem.set_half endian (addr + lo_off) lo
        Mem.set_half endian (addr + hi_off) hi

def Mem.get_wide [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (endian: Endian) (addr: Nat): M UInt64
  := do let (lo_off, hi_off)
          := match endian with
              | .Big => (4, 0)
              | .Little => (0, 4)
        let lo := (<- Mem.get_word endian (addr + lo_off)).toUInt64
        let hi := (<- Mem.get_word endian (addr + hi_off)).toUInt64
        pure ((hi <<< 32) ||| lo)

def Mem.set_wide [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (endian: Endian) (addr: Nat) (val: UInt64): M Unit
  := do let (lo_off, hi_off)
          := match endian with
              | .Big => (4, 0)
              | .Little => (0, 4)
        let lo := (val &&& 0xffffffff).toNat.toUInt32
        let hi := ((val >>> 32) &&& 0xffffffff).toNat.toUInt32
        Mem.set_word endian (addr + lo_off) lo
        Mem.set_word endian (addr + hi_off) hi

end RiscV.Mach.Mem
