/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy

namespace RiscV.Instr.Types

open R0sy.Data.Bits

def funct7_mask: UInt32 := Bits.mask 31 25
def funct3_mask: UInt32 := Bits.mask 14 12
def opcode_mask: UInt32 := Bits.mask 6 0


structure RCode where
  funct7: Bits 31 25
  funct3: Bits 14 12
  opcode: Bits 6 0

def RCode.new (funct7 funct3 opcode: UInt32): RCode
  := {
    funct7 := { val := funct7 }
    funct3 := { val := funct3 }
    opcode := { val := opcode }
  }

def RCode.mask: UInt32 := funct7_mask ||| funct3_mask ||| opcode_mask

def RCode.pattern (x: RCode): UInt32
  := x.funct7.toUInt32 ||| x.funct3.toUInt32 ||| x.opcode.toUInt32


structure RArgs where
  instr: RCode
  rs2: Bits 24 20
  rs1: Bits 19 15
  rd: Bits 11 7

def RArgs.mkArgs (instr: RCode) (x: UInt32): RArgs
  := {
    instr
    rs2 := Bits.ofUInt32 x
    rs1 := Bits.ofUInt32 x
    rd := Bits.ofUInt32 x
  }


structure ICode where
  funct3: Bits 14 12
  opcode: Bits 6 0

def ICode.new (funct3 opcode: UInt32): ICode
  := {
    funct3 := { val := funct3 }
    opcode := { val := opcode }
  }

def ICode.mask: UInt32 := funct3_mask ||| opcode_mask

def ICode.pattern (x: ICode): UInt32
  := x.funct3.toUInt32 ||| x.opcode.toUInt32


structure IArgs where
  instr: ICode
  imm11_0: Bits 31 20
  rs1: Bits 19 15
  rd: Bits 11 7

def IArgs.mkArgs (instr: ICode) (x: UInt32): IArgs
  := {
    instr,
    imm11_0 := Bits.ofUInt32 x
    rs1 := Bits.ofUInt32 x
    rd := Bits.ofUInt32 x
  }


structure SCode where
  funct3: Bits 14 12
  opcode: Bits 6 0

def SCode.new (funct3 opcode: UInt32): SCode
  := {
    funct3 := { val := funct3 }
    opcode := { val := opcode }
  }

def SCode.mask: UInt32 := funct3_mask ||| opcode_mask

def SCode.pattern (x: SCode): UInt32
  := x.funct3.toUInt32 ||| x.opcode.toUInt32


structure SArgs where
  instr: SCode
  imm11_5: Bits 31 25
  rs2: Bits 24 20
  rs1: Bits 19 15
  imm4_0: Bits 11 7

def SArgs.mkArgs (instr: SCode) (x: UInt32): SArgs
  := {
    instr
    imm11_5 := Bits.ofUInt32 x
    rs2 := Bits.ofUInt32 x
    rs1 := Bits.ofUInt32 x
    imm4_0 := Bits.ofUInt32 x
  }


structure BCode where
  funct3: Bits 14 12
  opcode: Bits 6 0

def BCode.new (funct3 opcode: UInt32): BCode
  := {
    funct3 := { val := funct3 }
    opcode := { val := opcode }
  }

def BCode.mask: UInt32 := funct3_mask ||| opcode_mask

def BCode.pattern (x: BCode): UInt32
  := x.funct3.toUInt32 ||| x.opcode.toUInt32


structure BArgs where
  instr: BCode
  imm12: Bits 31 31
  imm10_5: Bits 30 25
  rs2: Bits 24 20
  rs1: Bits 19 15
  imm4_1: Bits 11 8
  imm11: Bits 7 7

def BArgs.mkArgs (instr: BCode) (x: UInt32): BArgs
  := {
    instr
    imm12 := Bits.ofUInt32 x
    imm10_5 := Bits.ofUInt32 x
    rs2 := Bits.ofUInt32 x
    rs1 := Bits.ofUInt32 x
    imm4_1 := Bits.ofUInt32 x
    imm11 := Bits.ofUInt32 x
  }


structure UCode where
  opcode: Bits 6 0

def UCode.new (opcode: UInt32): UCode
  := {
    opcode := { val := opcode }
  }

def UCode.mask: UInt32 := opcode_mask

def UCode.pattern (x: UCode): UInt32
  := x.opcode.toUInt32


structure UArgs where
  instr: UCode
  imm31_12: Bits 31 12
  rd: Bits 11 7

def UArgs.mkArgs (instr: UCode) (x: UInt32): UArgs
  := {
    instr
    imm31_12 := Bits.ofUInt32 x
    rd := Bits.ofUInt32 x
  }


structure JCode where
  opcode: Bits 6 0

def JCode.new (opcode: UInt32): JCode
  := {
    opcode := { val := opcode }
  }

def JCode.mask: UInt32 := opcode_mask

def JCode.pattern (x: JCode): UInt32
  := x.opcode.toUInt32


structure JArgs where
  instr: JCode
  imm20: Bits 31 31
  imm10_1: Bits 30 21
  imm11: Bits 20 20
  imm19_12: Bits 19 12
  rd: Bits 11 7

def JArgs.mkArgs (instr: JCode) (x: UInt32): JArgs
  := {
    instr
    imm20 := Bits.ofUInt32 x
    imm10_1 := Bits.ofUInt32 x
    imm11 := Bits.ofUInt32 x
    imm19_12 := Bits.ofUInt32 x
    rd := Bits.ofUInt32 x
  }


structure ConstCode where
  const31_20: Bits 31 20
  const19_15: Bits 19 15
  const14_12: Bits 14 12
  const11_7: Bits 11 7
  opcode: Bits 6 0

def ConstCode.new (const31_20 const19_15 const14_12 const11_7 opcode: UInt32): ConstCode
  := {
    const31_20 := { val := const31_20 },
    const19_15 := { val := const19_15 },
    const14_12 := { val := const14_12 },
    const11_7 := { val := const11_7 },
    opcode := { val := opcode }
  }

def ConstCode.mask: UInt32 := Bits.mask 31 0

def ConstCode.pattern (x: ConstCode): UInt32
  := x.const31_20.toUInt32 ||| x.const19_15.toUInt32 ||| x.const14_12.toUInt32 ||| x.const11_7.toUInt32 ||| x.opcode.toUInt32


inductive CodeType where
  | R
  | I
  | S
  | B
  | U
  | J
  | Const

def CodeType.mask (t: CodeType): UInt32
  := match t with
      | R => RCode.mask
      | I => ICode.mask
      | S => SCode.mask
      | B => BCode.mask
      | U => UCode.mask
      | J => JCode.mask
      | Const => ConstCode.mask

def CodeType.Code (t: CodeType): Type
  := match t with
      | R => RCode
      | I => ICode
      | S => SCode
      | B => BCode
      | U => UCode
      | J => JCode
      | Const => ConstCode

def CodeType.pattern (t: CodeType) (code: CodeType.Code t): UInt32
  := match t with
      | R => RCode.pattern code
      | I => ICode.pattern code
      | S => SCode.pattern code
      | B => BCode.pattern code
      | U => UCode.pattern code
      | J => JCode.pattern code
      | Const => ConstCode.pattern code

def CodeType.Args (t: CodeType): Type
  := match t with
      | R => RArgs
      | I => IArgs
      | S => SArgs
      | B => BArgs
      | U => UArgs
      | J => JArgs
      | Const => Unit

def CodeType.mkArgs: (t: CodeType) -> CodeType.Code t -> UInt32 -> CodeType.Args t
  | R, code, x => RArgs.mkArgs code x
  | I, code, x => IArgs.mkArgs code x
  | S, code, x => SArgs.mkArgs code x
  | B, code, x => BArgs.mkArgs code x
  | U, code, x => UArgs.mkArgs code x
  | J, code, x => JArgs.mkArgs code x
  | Const, _, _ => ()


structure Code where
  format: CodeType
  code: CodeType.Code format

class InstructionSet (Mnemonic: Type) where
  code (m: Mnemonic): Code

def InstructionSet.mask [InstructionSet Mnemonic] (m: Mnemonic): UInt32
  := CodeType.mask (InstructionSet.code m).format

def InstructionSet.pattern [InstructionSet Mnemonic] (m: Mnemonic): UInt32
  := CodeType.pattern (InstructionSet.code m).format (InstructionSet.code m).code

def InstructionSet.code_matches [InstructionSet Mnemonic] (m: Mnemonic) (x: UInt32): Bool
  := x &&& (InstructionSet.mask m) == InstructionSet.pattern m

def InstructionSet.mkArgs [InstructionSet Mnemonic] (m: Mnemonic) (x: UInt32): CodeType.Args (InstructionSet.code m).format
  := CodeType.mkArgs (InstructionSet.code m).format (InstructionSet.code m).code x

end RiscV.Instr.Types
