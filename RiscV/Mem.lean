/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace RiscV.Mem


structure Ptr where
  val: UInt32
  -- aligned: val &&& 3 == 0
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


inductive MemError where
  | OutOfBounds (addr: Ptr)

structure Mem where
  blocks: Array Block
  deriving Inhabited

def Mem.locate_block [Monad M] [MonadExceptOf MemError M] [MonadStateOf Mem M] (addr: Ptr): M Nat
  := do let self <- get
        for i in [0:self.blocks.size] do
          if self.blocks[i]!.contains addr then return i
        throw (MemError.OutOfBounds addr)

def Mem.get_word [Monad M] [MonadExceptOf MemError M] [MonadStateOf Mem M] (addr: Ptr): M UInt32
  := do let self <- get
        let idx <- Mem.locate_block addr
        pure <| self.blocks[idx]!.get_word addr

def Mem.set_word [Monad M] [MonadExceptOf MemError M] [MonadStateOf Mem M] (addr: Ptr) (val: UInt32): M Unit
  := do let self <- get
        let idx <- Mem.locate_block addr
        set {
          self with
          blocks := Array.setD self.blocks idx (self.blocks[idx]!.set_word addr val)
        }

end RiscV.Mem
