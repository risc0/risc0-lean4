/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV.Exception

namespace RiscV.Mem

open Exception

structure Ptr where
  val: UInt32
  -- aligned: val % 4 == 0
  deriving Inhabited


structure Block where
  base: Ptr
  data: Array UInt32
  deriving Inhabited

def Block.size_in_bytes (self: Block): UInt32 := 4 * self.data.size.toUInt32

def Block.end (self: Block): Ptr
  := {
    val := self.base.val + self.size_in_bytes
    -- aligned := sorry
  }

def Block.contains (self: Block) (addr: Ptr): Bool
  := self.base.val <= addr.val && addr.val < self.end.val

def Block.get_word (self: Block) (addr: Ptr): UInt32
  := Array.getD self.data ((addr.val - self.base.val) >>> 2).toNat 0

def Block.set_word (self: Block) (addr: Ptr) (val: UInt32): Block
  := {
    self with
    data := Array.setD self.data ((addr.val - self.base.val) >>> 2).toNat val
  }


structure Mem where
  blocks: Array Block
  deriving Inhabited

def Mem.locate_block [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (addr: Ptr): M Nat
  := do let self <- get
        for i in [0:self.blocks.size] do
          if self.blocks[i]!.contains addr then return i
        throw (RiscVException.PtrOutOfBounds addr.val)

def Mem.get_word [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (addr: Ptr): M UInt32
  := do let self <- get
        let idx <- Mem.locate_block addr
        pure <| self.blocks[idx]!.get_word addr

def Mem.set_word [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (addr: Ptr) (val: UInt32): M Unit
  := do let self <- get
        let idx <- Mem.locate_block addr
        set {
          self with
          blocks := Array.setD self.blocks idx (self.blocks[idx]!.set_word addr val)
        }

def Mem.get_byte [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (addr: UInt32): M UInt8
  := do let word <- Mem.get_word { val := addr &&& 0xfffffffc }
        pure <| (word >>> (8 * (addr % 4))).toNat.toUInt8

def Mem.set_byte [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (addr: UInt32) (val: UInt8): M Unit
  := do let base: Ptr := { val := addr &&& 0xfffffffc }
        let word <- Mem.get_word base
        let byte_offset := 8 * (addr % 4)
        let val': UInt32 := val.toNat.toUInt32 <<< byte_offset
        let mask: UInt32 := ~~~ (0xff <<< byte_offset)
        Mem.set_word base ((word &&& mask) ||| val')

def Mem.get_half [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (addr: UInt32): M UInt16
  := do let word <- Mem.get_word { val := addr &&& 0xfffffffc }
        pure <| (word >>> (8 * (addr % 4))).toNat.toUInt16

def Mem.set_half [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Mem M] (addr: UInt32) (val: UInt16): M Unit
  := do let base: Ptr := { val := addr &&& 0xfffffffc }
        let word <- Mem.get_word base
        let byte_offset := 8 * (addr % 4)
        let val': UInt32 := val.toNat.toUInt32 <<< byte_offset
        let mask: UInt32 := ~~~ (0xffff <<< byte_offset)
        Mem.set_word base ((word &&& mask) ||| val')

end RiscV.Mem
