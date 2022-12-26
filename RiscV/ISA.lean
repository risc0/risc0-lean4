/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV.Instr.ISA
import RiscV.Instr.RV32I
import RiscV.Instr.RV32M
import RiscV.Instr.Types
import RiscV.Mach.Mem
import RiscV.Mach.Reg
import RiscV.Monad

namespace RiscV.ISA

open RiscV.Instr
open RiscV.Instr.ISA
open RiscV.Monad


namespace RV32IM
  inductive Instr where
    | I (instr: RV32I.Instr)
    | M (instr: RV32M.Instr)

  @[always_inline, inline]
  def ISA: ISA where
    Mnemonic := Instr
    all := RV32I.ISA.all.map .I ++ RV32M.ISA.all.map .M
    toString
      | .I instr => RV32I.ISA.toString instr
      | .M instr => RV32M.ISA.toString instr
    encode_mnemonic
      | .I instr => RV32I.ISA.encode_mnemonic instr
      | .M instr => RV32M.ISA.encode_mnemonic instr
    run
      | .I instr => RV32I.ISA.run instr
      | .M instr => RV32M.ISA.run instr
end RV32IM

end RiscV.ISA
