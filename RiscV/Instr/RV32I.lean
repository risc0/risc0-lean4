/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import RiscV.Instr.Types
import RiscV.Mach.Int
import RiscV.Mach.Mem
import RiscV.Mach.Reg
import RiscV.Monad

namespace RiscV.Instr.RV32I

open R0sy.Data.Bits
open R0sy.Lean.UInt32
open RiscV.Instr.Types
open RiscV.Mach.Int
open RiscV.Mach.Mem
open RiscV.Mach.Reg
open RiscV.Monad

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

inductive Instr where
  | LUI   | AUIPC | JAL   | JALR  | BEQ   | BNE   | BLT   | BGE
  | BLTU  | BGEU  | LB    | LH    | LW    | LBU   | LHU   | SB
  | SH    | SW    | ADDI  | SLTI  | SLTIU | XORI  | ORI   | ANDI
  | SLLI  | SRLI  | SRAI  | ADD   | SUB   | SLL   | SLT   | SLTU
  | XOR   | SRL   | SRA   | OR    | AND   | FENCE | ECALL | EBREAK

def ISA: ISA where
  Mnemonic := Instr
  all := #[
    .LUI,   .AUIPC, .JAL,   .JALR,  .BEQ,   .BNE,   .BLT,   .BGE,
    .BLTU,  .BGEU,  .LB,    .LH,    .LW,    .LBU,   .LHU,   .SB,
    .SH,    .SW,    .ADDI,  .SLTI,  .SLTIU, .XORI,  .ORI,   .ANDI,
    .SLLI,  .SRLI,  .SRAI,  .ADD,   .SUB,   .SLL,   .SLT,   .SLTU,
    .XOR,   .SRL,   .SRA,   .OR,    .AND,   .FENCE, .ECALL, .EBREAK
  ]
  toString
    | .LUI => "LUI"   | .AUIPC => "AUIPC" | .JAL => "JAL"   | .JALR => "JALR" | .BEQ => "BEQ"     | .BNE => "BNE"     | .BLT => "BLT"     | .BGE => "BGE"
    | .BLTU => "BLTU" | .BGEU => "BGEU"   | .LB => "LB"     | .LH => "LH"     | .LW => "LW"       | .LBU => "LBU"     | .LHU => "LHU"     | .SB => "SB"
    | .SH => "SH"     | .SW => "SW"       | .ADDI => "ADDI" | .SLTI => "SLTI" | .SLTIU => "SLTIU" | .XORI => "XORI"   | .ORI => "ORI"     | .ANDI => "ANDI"
    | .SLLI => "SLLI" | .SRLI => "SRLI"   | .SRAI => "SRAI" | .ADD => "ADD"   | .SUB => "SUB"     | .SLL => "SLL"     | .SLT => "SLT"     | .SLTU => "SLTU"
    | .XOR => "XOR"   | .SRL => "SRL"     | .SRA => "SRA"   | .OR => "OR"     | .AND => "AND"     | .FENCE => "FENCE" | .ECALL => "ECALL" | .EBREAK => "EBREAK"
  encode_mnemonic (m: Instr)
    := match m with
        | .LUI    => .U <|  U.EncMnemonic.new                       0b0110111
        | .AUIPC  => .U <|  U.EncMnemonic.new                       0b0010111
        | .JAL    => .J <|  J.EncMnemonic.new                       0b1101111
        | .JALR   => .I <|  I.EncMnemonic.new               0b000   0b1100111
        | .BEQ    => .B <|  B.EncMnemonic.new               0b000   0b1100011
        | .BNE    => .B <|  B.EncMnemonic.new               0b001   0b1100011
        | .BLT    => .B <|  B.EncMnemonic.new               0b100   0b1100011
        | .BGE    => .B <|  B.EncMnemonic.new               0b101   0b1100011
        | .BLTU   => .B <|  B.EncMnemonic.new               0b110   0b1100011
        | .BGEU   => .B <|  B.EncMnemonic.new               0b111   0b1100011
        | .LB     => .I <|  I.EncMnemonic.new               0b000   0b0000011
        | .LH     => .I <|  I.EncMnemonic.new               0b001   0b0000011
        | .LW     => .I <|  I.EncMnemonic.new               0b010   0b0000011
        | .LBU    => .I <|  I.EncMnemonic.new               0b100   0b0000011
        | .LHU    => .I <|  I.EncMnemonic.new               0b101   0b0000011
        | .SB     => .S <|  S.EncMnemonic.new               0b000   0b0100011
        | .SH     => .S <|  S.EncMnemonic.new               0b001   0b0100011
        | .SW     => .S <|  S.EncMnemonic.new               0b010   0b0100011
        | .ADDI   => .I <|  I.EncMnemonic.new               0b000   0b0010011
        | .SLTI   => .I <|  I.EncMnemonic.new               0b010   0b0010011
        | .SLTIU  => .I <|  I.EncMnemonic.new               0b011   0b0010011
        | .XORI   => .I <|  I.EncMnemonic.new               0b100   0b0010011
        | .ORI    => .I <|  I.EncMnemonic.new               0b110   0b0010011
        | .ANDI   => .I <|  I.EncMnemonic.new               0b111   0b0010011
        | .SLLI   => .R <|  R.EncMnemonic.new   0b0000000   0b001   0b0010011
        | .SRLI   => .R <|  R.EncMnemonic.new   0b0000000   0b101   0b0010011
        | .SRAI   => .R <|  R.EncMnemonic.new   0b0100000   0b101   0b0010011
        | .ADD    => .R <|  R.EncMnemonic.new   0b0000000   0b000   0b0110011
        | .SUB    => .R <|  R.EncMnemonic.new   0b0100000   0b000   0b0110011
        | .SLL    => .R <|  R.EncMnemonic.new   0b0000000   0b001   0b0110011
        | .SLT    => .R <|  R.EncMnemonic.new   0b0000000   0b010   0b0110011
        | .SLTU   => .R <|  R.EncMnemonic.new   0b0000000   0b011   0b0110011
        | .XOR    => .R <|  R.EncMnemonic.new   0b0000000   0b100   0b0110011
        | .SRL    => .R <|  R.EncMnemonic.new   0b0000000   0b101   0b0110011
        | .SRA    => .R <|  R.EncMnemonic.new   0b0100000   0b101   0b0110011
        | .OR     => .R <|  R.EncMnemonic.new   0b0000000   0b110   0b0110011
        | .AND    => .R <|  R.EncMnemonic.new   0b0000000   0b111   0b0110011
        | .FENCE  => .I <|  I.EncMnemonic.new               0b000   0b0001111
        | .ECALL  => .Const <|  Const.EncMnemonic.new   0b000000000000    0b00000   0b000   0b00000   0b1110011
        | .EBREAK => .Const <|  Const.EncMnemonic.new   0b000000000001    0b00000   0b000   0b00000   0b1110011
  run
    | .LUI, args
        => RegFile.set_word args.rd args.imm
    | .AUIPC, args
        => do let pc <- RegFile.get_word .PC
              RegFile.set_word .PC (pc + args.imm)
    | .JAL, args
        => do let pc <- RegFile.get_word .PC
              let newPC := pc + args.imm - 4
              if newPC % 4 != 0 then throw (.InstructionAddressMisaligned newPC)
              RegFile.set_word args.rd (pc + 4)
              RegFile.set_word .PC newPC
    | .JALR, args
        => do let pc <- RegFile.get_word .PC
              let newPC := pc + args.imm - 4
              if newPC % 4 != 0 then throw (.InstructionAddressMisaligned newPC)
              RegFile.set_word args.rd (pc + 4)
              RegFile.set_word .PC newPC
    | .BEQ, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              let pc <- RegFile.get_word .PC
              if x == y then do
                let newPC := pc + args.imm - 4
                if newPC % 4 != 0 then throw (.InstructionAddressMisaligned newPC)
                RegFile.set_word .PC newPC
    | .BNE, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              let pc <- RegFile.get_word .PC
              if x != y then do
                let newPC := pc + args.imm - 4
                if newPC % 4 != 0 then throw (.InstructionAddressMisaligned newPC)
                RegFile.set_word .PC newPC
    | .BLT, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              let pc <- RegFile.get_word .PC
              if UInt32.lt_signed x y then do
                let newPC := pc + args.imm - 4
                if newPC % 4 != 0 then throw (.InstructionAddressMisaligned newPC)
                RegFile.set_word .PC newPC
    | .BGE, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              let pc <- RegFile.get_word .PC
              if UInt32.ge_signed x y then do
                let newPC := pc + args.imm - 4
                if newPC % 4 != 0 then throw (.InstructionAddressMisaligned newPC)
                RegFile.set_word .PC newPC
    | .BLTU, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              let pc <- RegFile.get_word .PC
              if x < y then do
                let newPC := pc + args.imm - 4
                if newPC % 4 != 0 then throw (.InstructionAddressMisaligned newPC)
                RegFile.set_word .PC newPC
    | .BGEU, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              let pc <- RegFile.get_word .PC
              if x >= y then do
                let newPC := pc + args.imm - 4
                if newPC % 4 != 0 then throw (.InstructionAddressMisaligned newPC)
                RegFile.set_word .PC newPC
    | .LB, args
        => do let a <- RegFile.get_word args.rs1
              let addr := a + args.imm
              let x <- Mem.get_byte addr
              RegFile.set_word args.rd (UInt32.ofUInt8_signed x)
    | .LH, args
        => do let a <- RegFile.get_word args.rs1
              let addr := a + args.imm
              let x <- Mem.get_half addr
              RegFile.set_word args.rd (UInt32.ofUInt16_signed x)
    | .LW, args
        => do let a <- RegFile.get_word args.rs1
              let addr := a + args.imm
              let x <- Mem.get_word { val := addr }
              RegFile.set_word args.rd x
    | .LBU, args
        => do let a <- RegFile.get_word args.rs1
              let addr := a + args.imm
              let x <- Mem.get_byte addr
              RegFile.set_word args.rd x.toNat.toUInt32
    | .LHU, args
        => do let a <- RegFile.get_word args.rs1
              let addr := a + args.imm
              let x <- Mem.get_half addr
              RegFile.set_word args.rd x.toNat.toUInt32
    | .SB, args
        => do let a <- RegFile.get_word args.rs1
              let addr := a + args.imm
              let x <- RegFile.get_word args.rs2
              Mem.set_byte addr x.toNat.toUInt8
    | .SH, args
        => do let a <- RegFile.get_word args.rs1
              let addr := a + args.imm
              let x <- RegFile.get_word args.rs2
              Mem.set_half addr x.toNat.toUInt16
    | .SW, args
        => do let a <- RegFile.get_word args.rs1
              let addr := a + args.imm
              let x <- RegFile.get_word args.rs2
              Mem.set_word { val := addr } x
    | .ADDI, args
        => do let x <- RegFile.get_word args.rs1
              RegFile.set_word args.rd (x + args.imm)
    | .SLTI, args
        => do let x <- RegFile.get_word args.rs1
              RegFile.set_word args.rd (if UInt32.lt_signed x args.imm then 1 else 0)
    | .SLTIU, args
        => do let x <- RegFile.get_word args.rs1
              RegFile.set_word args.rd (if x < args.imm then 1 else 0)
    | .XORI, args
        => do let x <- RegFile.get_word args.rs1
              RegFile.set_word args.rd (x ^^^ args.imm)
    | .ORI, args
        => do let x <- RegFile.get_word args.rs1
              RegFile.set_word args.rd (x ||| args.imm)
    | .ANDI, args
        => do let x <- RegFile.get_word args.rs1
              RegFile.set_word args.rd (x &&& args.imm)
    | .SLLI, args
        => do let x <- RegFile.get_word args.rs1
              let shamt6 := (Reg.index args.rs2).toUInt32
              RegFile.set_word args.rd (x <<< shamt6)
    | .SRLI, args
        => do let x <- RegFile.get_word args.rs1
              let shamt6 := (Reg.index args.rs2).toUInt32
              RegFile.set_word args.rd (x >>> shamt6)
    | .SRAI, args
        => do let x <- RegFile.get_word args.rs1
              let shamt6 := (Reg.index args.rs2).toUInt32
              RegFile.set_word args.rd (UInt32.shr_signed x shamt6)
    | .ADD, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              RegFile.set_word args.rd (x + y)
    | .SUB, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              RegFile.set_word args.rd (x - y)
    | .SLL, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              RegFile.set_word args.rd (x <<< y)
    | .SLT, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              RegFile.set_word args.rd (if UInt32.lt_signed x y then 1 else 0)
    | .SLTU, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              RegFile.set_word args.rd (if x < y then 1 else 0)
    | .XOR, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              RegFile.set_word args.rd (x ^^^ y)
    | .SRL, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              RegFile.set_word args.rd (x >>> y)
    | .SRA, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              RegFile.set_word args.rd (UInt32.shr_signed x y)
    | .OR, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              RegFile.set_word args.rd (x ||| y)
    | .AND, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              RegFile.set_word args.rd (x &&& y)
    | .FENCE, _args => pure ()
    | .ECALL, _args => pure ()
    | .EBREAK, _args => pure ()

end RiscV.Instr.RV32I
