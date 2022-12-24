/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV
import Zkvm.Platform.Mem

namespace Zkvm.Platform.Elf

open RiscV.Monad

def loadElf: Elf -> Machine .RV32IMle
  := RiscV.Elf.loadElfInfo
      {
        reg_file := RiscV.Mach.Reg.RegFile.new,
        mem := Zkvm.Platform.Mem.emptyMem
      }

end Zkvm.Platform.Elf
