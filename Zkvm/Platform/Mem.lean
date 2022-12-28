/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV

namespace Zkvm.Platform.Mem

open RiscV.Mach.Mem

structure Region where
  base: Nat
  size_in_bytes: Nat

def Region.zeroBlock (self: Region): Block
  := {
    base := self.base,
    data := Array.mkArray self.size_in_bytes 0
  }

def STACK: Region
  := {
    base := 0x00000000,
    size_in_bytes := 16 <<< 20
  }

def DATA: Region
  := {
    base := 0x01000000,
    size_in_bytes := 16 <<< 20
  }

def HEAP: Region
  := {
    base := 0x02000000,
    size_in_bytes := 32 <<< 20
  }

def INPUT: Region
  := {
    base := 0x04000000,
    size_in_bytes := 32 <<< 20
  }

def PROG: Region
  := {
    base := 0x06000000,
    size_in_bytes := 32 <<< 20
  }

def OUTPUT: Region
  := {
    base := 0x08000000,
    size_in_bytes := 32 <<< 20
  }

def COMMIT: Region
  := {
    base := 0x0A000000,
    size_in_bytes := 32 <<< 20
  }

def SYSTEM: Region
  := {
    base := 0x0C000000,
    size_in_bytes := 64 <<< 20
  }

def emptyMem: Mem
  := {
    endian := .Little,
    blocks := #[
      STACK.zeroBlock,
      DATA.zeroBlock,
      HEAP.zeroBlock,
      INPUT.zeroBlock,
      PROG.zeroBlock,
      OUTPUT.zeroBlock,
      COMMIT.zeroBlock,
      SYSTEM.zeroBlock
    ]
  }

end Zkvm.Platform.Mem
