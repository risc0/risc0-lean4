/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV.Instr.InstrRV32I
import RiscV.Instr.InstrRV32M
import RiscV.Instr.Sets
import RiscV.Instr.Types

namespace RiscV.Instr.RV32IM

open InstrRV32I
open InstrRV32M
open Sets
open Types

inductive RV32IM where
  | I (instr: RV32I)
  | M (instr: RV32M)

instance : ToString RV32IM where
  toString
    | .I instr => ToString.toString instr
    | .M instr => ToString.toString instr

instance : InstructionSet RV32IM where
  all := (@InstructionSet.all RV32I _).map RV32IM.I ++ (@InstructionSet.all RV32M _).map RV32IM.M
  encode_mnemonic
    | .I instr => InstructionSet.encode_mnemonic instr
    | .M instr => InstructionSet.encode_mnemonic instr
  run
    | .I instr => InstructionSet.run instr
    | .M instr => InstructionSet.run instr

end RiscV.Instr.RV32IM
