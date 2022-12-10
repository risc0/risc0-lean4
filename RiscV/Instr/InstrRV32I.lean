/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV.Instr.Types

namespace RiscV.Instr.InstrRV32I

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
  all := #[
    .LUI,   .AUIPC, .JAL,   .JALR,  .BEQ,   .BNE,   .BLT,   .BGE,
    .BLTU,  .BGEU,  .LB,    .LH,    .LW,    .LBU,   .LHU,   .SB,
    .SH,    .SW,    .ADDI,  .SLTI,  .SLTIU, .XORI,  .ORI,   .ANDI,
    .SLLI,  .SRLI,  .SRAI,  .ADD,   .SUB,   .SLL,   .SLT,   .SLTU,
    .XOR,   .SRL,   .SRA,   .OR,    .AND,   .FENCE, .ECALL, .EBREAK
  ]
  code (m: RV32I)
    := match m with
        | .LUI    => { type := .U, code := UCode.new                      0b0110111 }
        | .AUIPC  => { type := .U, code := UCode.new                      0b0010111 }
        | .JAL    => { type := .J, code := JCode.new                      0b1101111 }
        | .JALR   => { type := .I, code := ICode.new              0b000   0b1100111 }
        | .BEQ    => { type := .B, code := BCode.new              0b000   0b1100011 }
        | .BNE    => { type := .B, code := BCode.new              0b001   0b1100011 }
        | .BLT    => { type := .B, code := BCode.new              0b100   0b1100011 }
        | .BGE    => { type := .B, code := BCode.new              0b101   0b1100011 }
        | .BLTU   => { type := .B, code := BCode.new              0b110   0b1100011 }
        | .BGEU   => { type := .B, code := BCode.new              0b111   0b1100011 }
        | .LB     => { type := .I, code := ICode.new              0b000   0b0000011 }
        | .LH     => { type := .I, code := ICode.new              0b001   0b0000011 }
        | .LW     => { type := .I, code := ICode.new              0b010   0b0000011 }
        | .LBU    => { type := .I, code := ICode.new              0b100   0b0000011 }
        | .LHU    => { type := .I, code := ICode.new              0b101   0b0000011 }
        | .SB     => { type := .S, code := SCode.new              0b000   0b0100011 }
        | .SH     => { type := .S, code := SCode.new              0b001   0b0100011 }
        | .SW     => { type := .S, code := SCode.new              0b010   0b0100011 }
        | .ADDI   => { type := .I, code := ICode.new              0b000   0b0010011 }
        | .SLTI   => { type := .I, code := ICode.new              0b010   0b0010011 }
        | .SLTIU  => { type := .I, code := ICode.new              0b011   0b0010011 }
        | .XORI   => { type := .I, code := ICode.new              0b100   0b0010011 }
        | .ORI    => { type := .I, code := ICode.new              0b110   0b0010011 }
        | .ANDI   => { type := .I, code := ICode.new              0b111   0b0010011 }
        | .SLLI   => { type := .R, code := RCode.new  0b0000000   0b001   0b0010011 }
        | .SRLI   => { type := .R, code := RCode.new  0b0000000   0b101   0b0010011 }
        | .SRAI   => { type := .R, code := RCode.new  0b0100000   0b101   0b0010011 }
        | .ADD    => { type := .R, code := RCode.new  0b0000000   0b000   0b0110011 }
        | .SUB    => { type := .R, code := RCode.new  0b0100000   0b000   0b0110011 }
        | .SLL    => { type := .R, code := RCode.new  0b0000000   0b001   0b0110011 }
        | .SLT    => { type := .R, code := RCode.new  0b0000000   0b010   0b0110011 }
        | .SLTU   => { type := .R, code := RCode.new  0b0000000   0b011   0b0110011 }
        | .XOR    => { type := .R, code := RCode.new  0b0000000   0b100   0b0110011 }
        | .SRL    => { type := .R, code := RCode.new  0b0000000   0b101   0b0110011 }
        | .SRA    => { type := .R, code := RCode.new  0b0100000   0b101   0b0110011 }
        | .OR     => { type := .R, code := RCode.new  0b0000000   0b110   0b0110011 }
        | .AND    => { type := .R, code := RCode.new  0b0000000   0b111   0b0110011 }
        | .FENCE  => { type := .I, code := ICode.new              0b000   0b0001111 }
        | .ECALL  => { type := .Const, code := ConstCode.new  0b000000000000    0b00000   0b000   0b00000   0b1110011 }
        | .EBREAK => { type := .Const, code := ConstCode.new  0b000000000001    0b00000   0b000   0b00000   0b1110011 }
  run
    | .LUI, args => pure ()
    | .AUIPC, args => pure ()
    | .JAL, args => pure ()
    | .JALR, args => pure ()
    | .BEQ, args => pure ()
    | .BNE, args => pure ()
    | .BLT, args => pure ()
    | .BGE, args => pure ()
    | .BLTU, args => pure ()
    | .BGEU, args => pure ()
    | .LB, args => pure ()
    | .LH, args => pure ()
    | .LW, args => pure ()
    | .LBU, args => pure ()
    | .LHU, args => pure ()
    | .SB, args => pure ()
    | .SH, args => pure ()
    | .SW, args => pure ()
    | .ADDI, args => pure ()
    | .SLTI, args => pure ()
    | .SLTIU, args => pure ()
    | .XORI, args => pure ()
    | .ORI, args => pure ()
    | .ANDI, args => pure ()
    | .SLLI, args => pure ()
    | .SRLI, args => pure ()
    | .SRAI, args => pure ()
    | .ADD, args => pure ()
    | .SUB, args => pure ()
    | .SLL, args => pure ()
    | .SLT, args => pure ()
    | .SLTU, args => pure ()
    | .XOR, args => pure ()
    | .SRL, args => pure ()
    | .SRA, args => pure ()
    | .OR, args => pure ()
    | .AND, args => pure ()
    | .FENCE, args => pure ()
    | .ECALL, args => pure ()
    | .EBREAK, args => pure ()

end RiscV.Instr.InstrRV32I
