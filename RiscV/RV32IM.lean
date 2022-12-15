/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV.Instr.RV32I
import RiscV.Instr.RV32M
import RiscV.Instr.Types
import RiscV.Monad

namespace RiscV.RV32IM

open RiscV.Instr
open RiscV.Monad

inductive Instr where
  | I (instr: RV32I.Instr)
  | M (instr: RV32M.Instr)

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

end RiscV.RV32IM
