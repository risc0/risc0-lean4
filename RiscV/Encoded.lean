/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV.Bits
import RiscV.Instruction
import RiscV.Opcode

namespace RiscV.Encoded

open Bits
open Instruction
open Opcode


structure RTypeInstr where
  funct7: Bits 31 25
  funct3: Bits 14 12
  opcode: Bits 6 0

def RTypeInstr.new (funct7 funct3 opcode: UInt32): RTypeInstr
  := {
    funct7 := { val := funct7 }
    funct3 := { val := funct3 }
    opcode := { val := opcode }
  }

structure RType where
  instr: RTypeInstr
  rs2: Bits 24 20
  rs1: Bits 19 15
  rd: Bits 11 7

def RType.ofUInt32 (instr: RTypeInstr) (x: UInt32): RType
  := {
    instr
    rs2 := Bits.ofUInt32 x
    rs1 := Bits.ofUInt32 x
    rd := Bits.ofUInt32 x
  }


structure ITypeInstr where
  funct3: Bits 14 12
  opcode: Bits 6 0

def ITypeInstr.new (funct3 opcode: UInt32): ITypeInstr
  := {
    funct3 := { val := funct3 }
    opcode := { val := opcode }
  }

structure IType where
  instr: ITypeInstr
  imm11_0: Bits 31 20
  rs1: Bits 19 15
  rd: Bits 11 7

def IType.ofUInt32 (instr: ITypeInstr) (x: UInt32): IType
  := {
    instr,
    imm11_0 := Bits.ofUInt32 x
    rs1 := Bits.ofUInt32 x
    rd := Bits.ofUInt32 x
  }


structure STypeInstr where
  funct3: Bits 14 12
  opcode: Bits 6 0

def STypeInstr.new (funct3 opcode: UInt32): STypeInstr
  := {
    funct3 := { val := funct3 }
    opcode := { val := opcode }
  }

structure SType where
  instr: STypeInstr
  imm11_5: Bits 31 25
  rs2: Bits 24 20
  rs1: Bits 19 15
  imm4_0: Bits 11 7

def SType.ofUInt32 (instr: STypeInstr) (x: UInt32): SType
  := {
    instr
    imm11_5 := Bits.ofUInt32 x
    rs2 := Bits.ofUInt32 x
    rs1 := Bits.ofUInt32 x
    imm4_0 := Bits.ofUInt32 x
  }


structure BTypeInstr where
  funct3: Bits 14 12
  opcode: Bits 6 0

def BTypeInstr.new (funct3 opcode: UInt32): BTypeInstr
  := {
    funct3 := { val := funct3 }
    opcode := { val := opcode }
  }

structure BType where
  instr: BTypeInstr
  imm12: Bits 31 31
  imm10_5: Bits 30 25
  rs2: Bits 24 20
  rs1: Bits 19 15
  imm4_1: Bits 11 8
  imm11: Bits 7 7

def BType.ofUInt32 (instr: BTypeInstr) (x: UInt32): BType
  := {
    instr
    imm12 := Bits.ofUInt32 x
    imm10_5 := Bits.ofUInt32 x
    rs2 := Bits.ofUInt32 x
    rs1 := Bits.ofUInt32 x
    imm4_1 := Bits.ofUInt32 x
    imm11 := Bits.ofUInt32 x
  }


structure UTypeInstr where
  opcode: Bits 6 0

def UTypeInstr.new (opcode: UInt32): UTypeInstr
  := {
    opcode := { val := opcode }
  }

structure UType where
  instr: UTypeInstr
  imm31_12: Bits 31 12
  rd: Bits 11 7

def UType.ofUInt32 (instr: UTypeInstr) (x: UInt32): UType
  := {
    instr
    imm31_12 := Bits.ofUInt32 x
    rd := Bits.ofUInt32 x
  }


structure JTypeInstr where
  opcode: Bits 6 0

def JTypeInstr.new (opcode: UInt32): JTypeInstr
  := {
    opcode := { val := opcode }
  }

structure JType where
  instr: JTypeInstr
  imm20: Bits 31 31
  imm10_1: Bits 30 21
  imm11: Bits 20 20
  imm19_12: Bits 19 12
  rd: Bits 11 7

def JType.ofUInt32 (instr: JTypeInstr) (x: UInt32): JType
  := {
    instr
    imm20 := Bits.ofUInt32 x
    imm10_1 := Bits.ofUInt32 x
    imm11 := Bits.ofUInt32 x
    imm19_12 := Bits.ofUInt32 x
    rd := Bits.ofUInt32 x
  }


inductive Encoded where
  | R (enc: RType)
  | I (enc: IType)
  | S (enc: SType)
  | SB (enc: BType)
  | U (enc: UType)
  | UJ (enc: JType)
  | Const (enc: UInt32)

end RiscV.Encoded
