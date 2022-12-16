/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV.Instr.RV32I
import RiscV.Instr.RV32M
import RiscV.Instr.Types
import RiscV.Mach.Mem
import RiscV.Mach.Reg
import RiscV.Monad

namespace RiscV.ISA

open RiscV.Instr
open RiscV.Monad


namespace RV32IM
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
    run variant
      | .I instr => RV32I.ISA.run variant instr
      | .M instr => RV32M.ISA.run variant instr
end RV32IM

end RiscV.ISA


namespace RiscV.Monad

open RiscV.Mach.Mem
open RiscV.Mach.Reg

namespace Variant
  def isa (variant: Variant): ISA
    := match variant with
        | .RV32IMle => RiscV.ISA.RV32IM.ISA
end Variant

namespace MonadMachine
  def step [MonadMachine variant M]: M Unit
    := do let isa := variant.isa
          let pc <- RegFile.get_word .PC
          let instr <- Mem.get_word { val := pc }
          match isa.deserialize_mnemonic instr with
            | none => throw (.InvalidInstruction pc instr)
            | some mnemonic
                => do RegFile.set_word .PC (pc + 4)
                      let enc_args := isa.deserialize_args mnemonic instr
                      let args := isa.decode_args mnemonic enc_args
                      isa.run variant mnemonic args
end MonadMachine
end RiscV.Monad
