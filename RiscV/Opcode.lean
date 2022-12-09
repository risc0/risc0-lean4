/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV.Bits

namespace RiscV.Opcode

open Bits

def opcode_LUI: Bits 6 0 := Bits.ofUInt32 0b0110111
def opcode_AUIPC: Bits 6 0 := Bits.ofUInt32 0b0010111
def opcode_JAL: Bits 6 0 := Bits.ofUInt32 0b1101111

def opcode_JALR: Bits 6 0 := Bits.ofUInt32 0b1100111
def funct3_JALR: Bits 14 12 := Bits.ofUInt32 0b000

def opcode_BRANCH: Bits 6 0 := Bits.ofUInt32 0b1100011
def funct3_BEQ: Bits 14 12 := Bits.ofUInt32 0b000
def funct3_BNE: Bits 14 12 := Bits.ofUInt32 0b001
def funct3_BLT: Bits 14 12 := Bits.ofUInt32 0b100
def funct3_BGE: Bits 14 12 := Bits.ofUInt32 0b101
def funct3_BLTU: Bits 14 12 := Bits.ofUInt32 0b110
def funct3_BGEU: Bits 14 12 := Bits.ofUInt32 0b111

def opcode_LOAD: Bits 6 0 := Bits.ofUInt32 0b0000011
def funct3_LB: Bits 14 12 := Bits.ofUInt32 0b000
def funct3_LH: Bits 14 12 := Bits.ofUInt32 0b001
def funct3_LW: Bits 14 12 := Bits.ofUInt32 0b010
def funct3_LBU: Bits 14 12 := Bits.ofUInt32 0b100
def funct3_LHU: Bits 14 12 := Bits.ofUInt32 0b101

def opcode_STORE: Bits 6 0 := Bits.ofUInt32 0b0100011
def funct3_SB: Bits 14 12 := Bits.ofUInt32 0b000
def funct3_SH: Bits 14 12 := Bits.ofUInt32 0b001
def funct3_SW: Bits 14 12 := Bits.ofUInt32 0b010

def opcode_OP_IMM: Bits 6 0 := Bits.ofUInt32 0b0010011
def funct3_ADDI: Bits 14 12 := Bits.ofUInt32 0b000
def funct3_SLTI: Bits 14 12 := Bits.ofUInt32 0b010
def funct3_SLTIU: Bits 14 12 := Bits.ofUInt32 0b011
def funct3_XORI: Bits 14 12 := Bits.ofUInt32 0b100
def funct3_ORI: Bits 14 12 := Bits.ofUInt32 0b110
def funct3_ANDI: Bits 14 12 := Bits.ofUInt32 0b111

def opcode_OP: Bits 6 0 := Bits.ofUInt32 0b0110011
def funct3_SLLI: Bits 14 12 := Bits.ofUInt32 0b001
def funct7_SLLI: Bits 31 25 := Bits.ofUInt32 0b0000000
def funct3_SRLI: Bits 14 12 := Bits.ofUInt32 0b101
def funct7_SRLI: Bits 31 25 := Bits.ofUInt32 0b0000000
def funct3_SRAI: Bits 14 12 := Bits.ofUInt32 0b101
def funct7_SRAI: Bits 31 25 := Bits.ofUInt32 0b0100000
def funct3_ADD: Bits 14 12 := Bits.ofUInt32 0b000
def funct7_ADD: Bits 31 25 := Bits.ofUInt32 0b0000000
def funct3_SUB: Bits 14 12 := Bits.ofUInt32 0b000
def funct7_SUB: Bits 31 25 := Bits.ofUInt32 0b0100000
def funct3_SLL: Bits 14 12 := Bits.ofUInt32 0b001
def funct7_SLL: Bits 31 25 := Bits.ofUInt32 0b0000000
def funct3_SLT: Bits 14 12 := Bits.ofUInt32 0b010
def funct7_SLT: Bits 31 25 := Bits.ofUInt32 0b0000000
def funct3_SLTU: Bits 14 12 := Bits.ofUInt32 0b011
def funct7_SLTU: Bits 31 25 := Bits.ofUInt32 0b0000000
def funct3_XOR: Bits 14 12 := Bits.ofUInt32 0b100
def funct7_XOR: Bits 31 25 := Bits.ofUInt32 0b0000000
def funct3_SRL: Bits 14 12 := Bits.ofUInt32 0b101
def funct7_SRL: Bits 31 25 := Bits.ofUInt32 0b0000000
def funct3_SRA: Bits 14 12 := Bits.ofUInt32 0b101
def funct7_SRA: Bits 31 25 := Bits.ofUInt32 0b0100000
def funct3_OR: Bits 14 12 := Bits.ofUInt32 0b110
def funct7_OR: Bits 31 25 := Bits.ofUInt32 0b0000000
def funct3_AND: Bits 14 12 := Bits.ofUInt32 0b111
def funct7_AND: Bits 31 25 := Bits.ofUInt32 0b0000000

/- old -/

def opcode_LOAD_FP: Bits 6 0 := Bits.ofUInt32 0b0000111
def opcode_MISC_MEM: Bits 6 0 := Bits.ofUInt32 0b0001111
def opcode_OP_IMM_32: Bits 6 0 := Bits.ofUInt32 0b0011011

def opcode_STORE_FP: Bits 6 0 := Bits.ofUInt32 0b0100111
def opcode_AMO: Bits 6 0 := Bits.ofUInt32 0b0101111
def opcode_OP_32: Bits 6 0 := Bits.ofUInt32 0b0111011

def opcode_MADD: Bits 6 0 := Bits.ofUInt32 0b1000011
def opcode_MSUB: Bits 6 0 := Bits.ofUInt32 0b1000111
def opcode_NMSUB: Bits 6 0 := Bits.ofUInt32 0b1001011
def opcode_NMADD: Bits 6 0 := Bits.ofUInt32 0b1001111
def opcode_OP_FP: Bits 6 0 := Bits.ofUInt32 0b1010011

def opcode_SYSTEM: Bits 6 0 := Bits.ofUInt32 0b1110011

-- MISC_MEM sub-opcodes
def funct3_FENCE: Bits 14 12 := Bits.ofUInt32 0b000
def funct3_FENCE_I: Bits 14 12 := Bits.ofUInt32 0b001


-- OP sub-opcodes, M standard extension

def funct3_MUL: Bits 14 12 := Bits.ofUInt32 0b000
def funct7_MUL: Bits 31 25 := Bits.ofUInt32 0b0000001

def funct3_MULH: Bits 14 12 := Bits.ofUInt32 0b001
def funct7_MULH: Bits 31 25 := Bits.ofUInt32 0b0000001

def funct3_MULHSU: Bits 14 12 := Bits.ofUInt32 0b010
def funct7_MULHSU: Bits 31 25 := Bits.ofUInt32 0b0000001

def funct3_MULHU: Bits 14 12 := Bits.ofUInt32 0b011
def funct7_MULHU: Bits 31 25 := Bits.ofUInt32 0b0000001

def funct3_DIV: Bits 14 12 := Bits.ofUInt32 0b100
def funct7_DIV: Bits 31 25 := Bits.ofUInt32 0b0000001

def funct3_DIVU: Bits 14 12 := Bits.ofUInt32 0b101
def funct7_DIVU: Bits 31 25 := Bits.ofUInt32 0b0000001

def funct3_REM: Bits 14 12 := Bits.ofUInt32 0b110
def funct7_REM: Bits 31 25 := Bits.ofUInt32 0b0000001

def funct3_REMU: Bits 14 12 := Bits.ofUInt32 0b111
def funct7_REMU: Bits 31 25 := Bits.ofUInt32 0b0000001

end RiscV.Opcode
