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
    size_in_bytes := 9 <<< 20
  }

def DATA: Region
  := {
    base := 0x00900000,
    size_in_bytes := 1 <<< 20
  }

def HEAP: Region
  := {
    base := 0x00A00000,
    size_in_bytes := 20 <<< 20
  }

def INPUT: Region
  := {
    base := 0x01E00000,
    size_in_bytes := 1 <<< 20
  }

def GPIO: Region
  := {
    base := 0x01F00000,
    size_in_bytes := 1 <<< 20
  }

def PROG: Region
  := {
    base := 0x02000000,
    size_in_bytes := 10 <<< 20
  }

def SHA: Region
  := {
    base := 0x02A00000,
    size_in_bytes := 1 <<< 20
  }

def WOM: Region
  := {
    base := 0x02B00000,
    size_in_bytes := 21 <<< 20
  }

def OUTPUT: Region
  := {
    base := 0x02B00000,
    size_in_bytes := 20 <<< 20
  }

def COMMIT: Region
  := {
    base := 0x03F00000,
    size_in_bytes := 1 <<< 20
  }

def SYSTEM: Region
  := {
    base := 0x0C000000,
    size_in_bytes := 64 <<< 20
  }

def FFPU: Region
  := {
    base := 0x0C000000 + 192 * 4
    size_in_bytes := 64 <<< 20 - 192 * 4
  }

def emptyMem: Mem
  := {
    endian := .Little,
    blocks := #[
      STACK.zeroBlock,
      DATA.zeroBlock,
      HEAP.zeroBlock,
      INPUT.zeroBlock,
      GPIO.zeroBlock,
      PROG.zeroBlock,
      SHA.zeroBlock,
      WOM.zeroBlock,
      OUTPUT.zeroBlock,
      COMMIT.zeroBlock,
      SYSTEM.zeroBlock,
      FFPU.zeroBlock
    ]
  }

end Zkvm.Platform.Mem
