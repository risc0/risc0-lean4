/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV.Instr.InstrRV32I
import RiscV.Instr.InstrRV32M
import RiscV.Instr.Types

namespace RiscV.Instr.RV32IM

open InstrRV32I
open InstrRV32M
open Types

inductive RV32IM where
  | I (instr: RV32I)
  | M (instr: RV32M)

instance : InstructionSet RV32IM where
  all := (@InstructionSet.all RV32I _).map RV32IM.I ++ (@InstructionSet.all RV32M _).map RV32IM.M
  code
    | .I instr => InstructionSet.code instr
    | .M instr => InstructionSet.code instr
  run
    | .I instr => InstructionSet.run instr
    | .M instr => InstructionSet.run instr

end RiscV.Instr.RV32IM
