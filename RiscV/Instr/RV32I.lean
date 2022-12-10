/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV.Instr.Types

namespace RiscV.Instr.RV32I

open Types

/-
Volume I: RISC-V Unprivileged ISA V20191213
RV32I Base Instruction Set

funct7          rs2   rs1   funct3          rd    opcode    R-type
imm[11:0]             rs1   funct3          rd    opcode    I-type
imm[11:5]       rs2   rs1   funct3     imm[4:0]   opcode    S-type
imm[12|10:5]    rs2   rs1   funct3  imm[4:1|11]   opcode    B-type
imm[31:12]                                  rd    opcode    U-type
imm[20|10:1|11|19:12]                       rd    opcode    J-type

imm[31:12]                                  rd    0110111   LUI
imm[31:12]                                  rd    0010111   AUIPC
imm[20|10:1|11|19:12]                       rd    1101111   JAL
imm[11:0]             rs1   000             rd    1100111   JALR
imm[12|10:5]    rs2   rs1   000     imm[4:1|11]   1100011   BEQ
imm[12|10:5]    rs2   rs1   001     imm[4:1|11]   1100011   BNE
imm[12|10:5]    rs2   rs1   100     imm[4:1|11]   1100011   BLT
imm[12|10:5]    rs2   rs1   101     imm[4:1|11]   1100011   BGE
imm[12|10:5]    rs2   rs1   110     imm[4:1|11]   1100011   BLTU
imm[12|10:5]    rs2   rs1   111     imm[4:1|11]   1100011   BGEU
imm[11:0]             rs1   000             rd    0000011   LB
imm[11:0]             rs1   001             rd    0000011   LH
imm[11:0]             rs1   010             rd    0000011   LW
imm[11:0]             rs1   100             rd    0000011   LBU
imm[11:0]             rs1   101             rd    0000011   LHU
imm[11:5]       rs2   rs1   000       imm[4:0]    0100011   SB
imm[11:5]       rs2   rs1   001       imm[4:0]    0100011   SH
imm[11:5]       rs2   rs1   010       imm[4:0]    0100011   SW
imm[11:0]             rs1   000             rd    0010011   ADDI
imm[11:0]             rs1   010             rd    0010011   SLTI
imm[11:0]             rs1   011             rd    0010011   SLTIU
imm[11:0]             rs1   100             rd    0010011   XORI
imm[11:0]             rs1   110             rd    0010011   ORI
imm[11:0]             rs1   111             rd    0010011   ANDI
0000000       shamt   rs1   001             rd    0010011   SLLI
0000000       shamt   rs1   101             rd    0010011   SRLI
0100000       shamt   rs1   101             rd    0010011   SRAI
0000000         rs2   rs1   000             rd    0110011   ADD
0100000         rs2   rs1   000             rd    0110011   SUB
0000000         rs2   rs1   001             rd    0110011   SLL
0000000         rs2   rs1   010             rd    0110011   SLT
0000000         rs2   rs1   011             rd    0110011   SLTU
0000000         rs2   rs1   100             rd    0110011   XOR
0000000         rs2   rs1   101             rd    0110011   SRL
0100000         rs2   rs1   101             rd    0110011   SRA
0000000         rs2   rs1   110             rd    0110011   OR
0000000         rs2   rs1   111             rd    0110011   AND
fm    pred     succ   rs1   000             rd    0001111   FENCE
000000000000    00000       000          00000    1110011   ECALL
000000000001    00000       000          00000    1110011   EBREAK
-/

inductive RV32I where
  | LUI   | AUIPC | JAL   | JALR  | BEQ   | BNE   | BLT   | BGE
  | BLTU  | BGEU  | LB    | LH    | LW    | LBU   | LHU   | SB
  | SH    | SW    | ADDI  | SLTI  | SLTIU | XORI  | ORI   | ANDI
  | SLLI  | SRLI  | SRAI  | ADD   | SUB   | SLL   | SLT   | SLTU
  | XOR   | SRL   | SRA   | OR    | AND   | FENCE | ECALL | EBREAK

instance : InstructionSet RV32I where
  code (m: RV32I)
    := match m with
        | .LUI    => { format := CodeType.U, code := UCode.new                      0b0110111 }
        | .AUIPC  => { format := CodeType.U, code := UCode.new                      0b0010111 }
        | .JAL    => { format := CodeType.J, code := JCode.new                      0b1101111 }
        | .JALR   => { format := CodeType.I, code := ICode.new              0b000   0b1100111 }
        | .BEQ    => { format := CodeType.B, code := BCode.new              0b000   0b1100011 }
        | .BNE    => { format := CodeType.B, code := BCode.new              0b001   0b1100011 }
        | .BLT    => { format := CodeType.B, code := BCode.new              0b100   0b1100011 }
        | .BGE    => { format := CodeType.B, code := BCode.new              0b101   0b1100011 }
        | .BLTU   => { format := CodeType.B, code := BCode.new              0b110   0b1100011 }
        | .BGEU   => { format := CodeType.B, code := BCode.new              0b111   0b1100011 }
        | .LB     => { format := CodeType.I, code := ICode.new              0b000   0b0000011 }
        | .LH     => { format := CodeType.I, code := ICode.new              0b001   0b0000011 }
        | .LW     => { format := CodeType.I, code := ICode.new              0b010   0b0000011 }
        | .LBU    => { format := CodeType.I, code := ICode.new              0b100   0b0000011 }
        | .LHU    => { format := CodeType.I, code := ICode.new              0b101   0b0000011 }
        | .SB     => { format := CodeType.S, code := SCode.new              0b000   0b0100011 }
        | .SH     => { format := CodeType.S, code := SCode.new              0b001   0b0100011 }
        | .SW     => { format := CodeType.S, code := SCode.new              0b010   0b0100011 }
        | .ADDI   => { format := CodeType.I, code := ICode.new              0b000   0b0010011 }
        | .SLTI   => { format := CodeType.I, code := ICode.new              0b010   0b0010011 }
        | .SLTIU  => { format := CodeType.I, code := ICode.new              0b011   0b0010011 }
        | .XORI   => { format := CodeType.I, code := ICode.new              0b100   0b0010011 }
        | .ORI    => { format := CodeType.I, code := ICode.new              0b110   0b0010011 }
        | .ANDI   => { format := CodeType.I, code := ICode.new              0b111   0b0010011 }
        | .SLLI   => { format := CodeType.R, code := RCode.new  0b0000000   0b001   0b0010011 }
        | .SRLI   => { format := CodeType.R, code := RCode.new  0b0000000   0b101   0b0010011 }
        | .SRAI   => { format := CodeType.R, code := RCode.new  0b0100000   0b101   0b0010011 }
        | .ADD    => { format := CodeType.R, code := RCode.new  0b0000000   0b000   0b0110011 }
        | .SUB    => { format := CodeType.R, code := RCode.new  0b0100000   0b000   0b0110011 }
        | .SLL    => { format := CodeType.R, code := RCode.new  0b0000000   0b001   0b0110011 }
        | .SLT    => { format := CodeType.R, code := RCode.new  0b0000000   0b010   0b0110011 }
        | .SLTU   => { format := CodeType.R, code := RCode.new  0b0000000   0b011   0b0110011 }
        | .XOR    => { format := CodeType.R, code := RCode.new  0b0000000   0b100   0b0110011 }
        | .SRL    => { format := CodeType.R, code := RCode.new  0b0000000   0b101   0b0110011 }
        | .SRA    => { format := CodeType.R, code := RCode.new  0b0100000   0b101   0b0110011 }
        | .OR     => { format := CodeType.R, code := RCode.new  0b0000000   0b110   0b0110011 }
        | .AND    => { format := CodeType.R, code := RCode.new  0b0000000   0b111   0b0110011 }
        | .FENCE  => { format := CodeType.I, code := ICode.new              0b000   0b0001111 }
        | .ECALL  => { format := CodeType.Const, code := ConstCode.new  0b000000000000    0b00000   0b000   0b00000   0b1110011 }
        | .EBREAK => { format := CodeType.Const, code := ConstCode.new  0b000000000001    0b00000   0b000   0b00000   0b1110011 }

end RiscV.Instr.RV32I
