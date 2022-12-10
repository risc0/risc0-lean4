/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV.Instr.Types

namespace RiscV.Instr.RV32M

open Types

/-
Volume I: RISC-V Unprivileged ISA V20191213
RV32M Standard Extension

funct7    rs2   rs1   funct3  rd    opcode    R-type 

0000001   rs2   rs1   000     rd    0110011   MUL
0000001   rs2   rs1   001     rd    0110011   MULH
0000001   rs2   rs1   010     rd    0110011   MULHSU
0000001   rs2   rs1   011     rd    0110011   MULHU
0000001   rs2   rs1   100     rd    0110011   DIV
0000001   rs2   rs1   101     rd    0110011   DIVU
0000001   rs2   rs1   110     rd    0110011   REM
0000001   rs2   rs1   111     rd    0110011   REMU
-/

inductive RV32M where
  | MUL | MULH | MULHSU | MULHU | DIV | DIVU | REM | REMU

instance : InstructionSet RV32M where
  code (m: RV32M)
    := match m with
        | .MUL =>    { format := CodeType.R, code := RCode.new  0b0000001   0b000   0b0110011 }
        | .MULH =>   { format := CodeType.R, code := RCode.new  0b0000001   0b001   0b0110011 }
        | .MULHSU => { format := CodeType.R, code := RCode.new  0b0000001   0b010   0b0110011 }
        | .MULHU =>  { format := CodeType.R, code := RCode.new  0b0000001   0b011   0b0110011 }
        | .DIV =>    { format := CodeType.R, code := RCode.new  0b0000001   0b100   0b0110011 }
        | .DIVU =>   { format := CodeType.R, code := RCode.new  0b0000001   0b101   0b0110011 }
        | .REM =>    { format := CodeType.R, code := RCode.new  0b0000001   0b110   0b0110011 }
        | .REMU =>   { format := CodeType.R, code := RCode.new  0b0000001   0b111   0b0110011 }

end RiscV.Instr.RV32M
