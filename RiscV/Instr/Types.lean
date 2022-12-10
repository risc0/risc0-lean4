/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import RiscV.Monad

namespace RiscV.Instr.Types

open R0sy.Data.Bits
open Monad


namespace Masks
  def funct7_mask: UInt32 := Bits.mask 31 25
  def funct3_mask: UInt32 := Bits.mask 14 12
  def opcode_mask: UInt32 := Bits.mask 6 0
end Masks


namespace R
  structure EncMnemonic where
    funct7: Bits 31 25
    funct3: Bits 14 12
    opcode: Bits 6 0

  def EncMnemonic.new (funct7 funct3 opcode: UInt32): EncMnemonic
    := {
      funct7 := { val := funct7 }
      funct3 := { val := funct3 }
      opcode := { val := opcode }
    }

  def EncMnemonic.mask: UInt32 := Masks.funct7_mask ||| Masks.funct3_mask ||| Masks.opcode_mask

  def EncMnemonic.pattern (x: EncMnemonic): UInt32
    := x.funct7.toUInt32 ||| x.funct3.toUInt32 ||| x.opcode.toUInt32


  structure EncArgs where
    rs2: Bits 24 20
    rs1: Bits 19 15
    rd: Bits 11 7

  def EncArgs.mkEncArgs (x: UInt32): EncArgs
    := {
      rs2 := Bits.ofUInt32 x
      rs1 := Bits.ofUInt32 x
      rd := Bits.ofUInt32 x
    }
end R


namespace I
  structure EncMnemonic where
    funct3: Bits 14 12
    opcode: Bits 6 0

  def EncMnemonic.new (funct3 opcode: UInt32): EncMnemonic
    := {
      funct3 := { val := funct3 }
      opcode := { val := opcode }
    }

  def EncMnemonic.mask: UInt32 := Masks.funct3_mask ||| Masks.opcode_mask

  def EncMnemonic.pattern (x: EncMnemonic): UInt32
    := x.funct3.toUInt32 ||| x.opcode.toUInt32


  structure EncArgs where
    imm11_0: Bits 31 20
    rs1: Bits 19 15
    rd: Bits 11 7

  def EncArgs.mkEncArgs (x: UInt32): EncArgs
    := {
      imm11_0 := Bits.ofUInt32 x
      rs1 := Bits.ofUInt32 x
      rd := Bits.ofUInt32 x
    }
end I


namespace S
  structure EncMnemonic where
    funct3: Bits 14 12
    opcode: Bits 6 0

  def EncMnemonic.new (funct3 opcode: UInt32): EncMnemonic
    := {
      funct3 := { val := funct3 }
      opcode := { val := opcode }
    }

  def EncMnemonic.mask: UInt32 := Masks.funct3_mask ||| Masks.opcode_mask

  def EncMnemonic.pattern (x: EncMnemonic): UInt32
    := x.funct3.toUInt32 ||| x.opcode.toUInt32


  structure EncArgs where
    imm11_5: Bits 31 25
    rs2: Bits 24 20
    rs1: Bits 19 15
    imm4_0: Bits 11 7

  def EncArgs.mkEncArgs (x: UInt32): EncArgs
    := {
      imm11_5 := Bits.ofUInt32 x
      rs2 := Bits.ofUInt32 x
      rs1 := Bits.ofUInt32 x
      imm4_0 := Bits.ofUInt32 x
    }
end S


namespace B
  structure EncMnemonic where
    funct3: Bits 14 12
    opcode: Bits 6 0

  def EncMnemonic.new (funct3 opcode: UInt32): EncMnemonic
    := {
      funct3 := { val := funct3 }
      opcode := { val := opcode }
    }

  def EncMnemonic.mask: UInt32 := Masks.funct3_mask ||| Masks.opcode_mask

  def EncMnemonic.pattern (x: EncMnemonic): UInt32
    := x.funct3.toUInt32 ||| x.opcode.toUInt32


  structure EncArgs where
    imm12: Bits 31 31
    imm10_5: Bits 30 25
    rs2: Bits 24 20
    rs1: Bits 19 15
    imm4_1: Bits 11 8
    imm11: Bits 7 7

  def EncArgs.mkEncArgs (x: UInt32): EncArgs
    := {
      imm12 := Bits.ofUInt32 x
      imm10_5 := Bits.ofUInt32 x
      rs2 := Bits.ofUInt32 x
      rs1 := Bits.ofUInt32 x
      imm4_1 := Bits.ofUInt32 x
      imm11 := Bits.ofUInt32 x
    }
end B


namespace U
  structure EncMnemonic where
    opcode: Bits 6 0

  def EncMnemonic.new (opcode: UInt32): EncMnemonic
    := {
      opcode := { val := opcode }
    }

  def EncMnemonic.mask: UInt32 := Masks.opcode_mask

  def EncMnemonic.pattern (x: EncMnemonic): UInt32
    := x.opcode.toUInt32


  structure EncArgs where
    imm31_12: Bits 31 12
    rd: Bits 11 7

  def EncArgs.mkEncArgs (x: UInt32): EncArgs
    := {
      imm31_12 := Bits.ofUInt32 x
      rd := Bits.ofUInt32 x
    }
end U


namespace J
  structure EncMnemonic where
    opcode: Bits 6 0

  def EncMnemonic.new (opcode: UInt32): EncMnemonic
    := {
      opcode := { val := opcode }
    }

  def EncMnemonic.mask: UInt32 := Masks.opcode_mask

  def EncMnemonic.pattern (x: EncMnemonic): UInt32
    := x.opcode.toUInt32


  structure EncArgs where
    imm20: Bits 31 31
    imm10_1: Bits 30 21
    imm11: Bits 20 20
    imm19_12: Bits 19 12
    rd: Bits 11 7

  def EncArgs.mkEncArgs (x: UInt32): EncArgs
    := {
      imm20 := Bits.ofUInt32 x
      imm10_1 := Bits.ofUInt32 x
      imm11 := Bits.ofUInt32 x
      imm19_12 := Bits.ofUInt32 x
      rd := Bits.ofUInt32 x
    }
end J


namespace Const
  structure EncMnemonic where
    const31_20: Bits 31 20
    const19_15: Bits 19 15
    const14_12: Bits 14 12
    const11_7: Bits 11 7
    opcode: Bits 6 0

  def EncMnemonic.new (const31_20 const19_15 const14_12 const11_7 opcode: UInt32): EncMnemonic
    := {
      const31_20 := { val := const31_20 },
      const19_15 := { val := const19_15 },
      const14_12 := { val := const14_12 },
      const11_7 := { val := const11_7 },
      opcode := { val := opcode }
    }

  def EncMnemonic.mask: UInt32 := Bits.mask 31 0

  def EncMnemonic.pattern (x: EncMnemonic): UInt32
    := x.const31_20.toUInt32 ||| x.const19_15.toUInt32 ||| x.const14_12.toUInt32 ||| x.const11_7.toUInt32 ||| x.opcode.toUInt32
end Const


inductive EncType where
  | R
  | I
  | S
  | B
  | U
  | J
  | Const

def EncType.mask (t: EncType): UInt32
  := match t with
      | R => R.EncMnemonic.mask
      | I => I.EncMnemonic.mask
      | S => S.EncMnemonic.mask
      | B => B.EncMnemonic.mask
      | U => U.EncMnemonic.mask
      | J => J.EncMnemonic.mask
      | Const => Const.EncMnemonic.mask

def EncType.EncMnemonic (t: EncType): Type
  := match t with
      | R => R.EncMnemonic
      | I => I.EncMnemonic
      | S => S.EncMnemonic
      | B => B.EncMnemonic
      | U => U.EncMnemonic
      | J => J.EncMnemonic
      | Const => Const.EncMnemonic

def EncType.pattern {t: EncType} (code: EncType.EncMnemonic t): UInt32
  := match t with
      | R => R.EncMnemonic.pattern code
      | I => I.EncMnemonic.pattern code
      | S => S.EncMnemonic.pattern code
      | B => B.EncMnemonic.pattern code
      | U => U.EncMnemonic.pattern code
      | J => J.EncMnemonic.pattern code
      | Const => Const.EncMnemonic.pattern code

def EncType.EncArgs (t: EncType): Type
  := match t with
      | R => R.EncArgs
      | I => I.EncArgs
      | S => S.EncArgs
      | B => B.EncArgs
      | U => U.EncArgs
      | J => J.EncArgs
      | Const => Unit

def EncType.mkEncArgs: {t: EncType} -> UInt32 -> EncType.EncArgs t
  | R, x => R.EncArgs.mkEncArgs x
  | I, x => I.EncArgs.mkEncArgs x
  | S, x => S.EncArgs.mkEncArgs x
  | B, x => B.EncArgs.mkEncArgs x
  | U, x => U.EncArgs.mkEncArgs x
  | J, x => J.EncArgs.mkEncArgs x
  | Const, _ => ()


structure EncMnemonic where
  type: EncType
  encoded_mnemonic: EncType.EncMnemonic type


class InstructionSet (Mnemonic: Type) where
  all: Array Mnemonic
  encode_mnemonic (m: Mnemonic): EncMnemonic
  run [MonadMachine M] (m: Mnemonic) (args: EncType.EncArgs (encode_mnemonic m).type): M Unit

def InstructionSet.mask [InstructionSet Mnemonic] (m: Mnemonic): UInt32
  := EncType.mask (InstructionSet.encode_mnemonic m).type

def InstructionSet.pattern [InstructionSet Mnemonic] (m: Mnemonic): UInt32
  := EncType.pattern (InstructionSet.encode_mnemonic m).encoded_mnemonic

def InstructionSet.code_matches [InstructionSet Mnemonic] (m: Mnemonic) (x: UInt32): Bool
  := x &&& (InstructionSet.mask m) == InstructionSet.pattern m

def InstructionSet.decode (Mnemonic: Type) [InstructionSet Mnemonic] (x: UInt32): Option Mnemonic
  := Id.run do
      for mnemonic in @InstructionSet.all Mnemonic _ do
        if InstructionSet.code_matches mnemonic x then return (some mnemonic)
      pure none

def InstructionSet.EncArgs [InstructionSet Mnemonic] (m: Mnemonic): Type
  := EncType.EncArgs (InstructionSet.encode_mnemonic m).type

def InstructionSet.mkEncArgs [InstructionSet Mnemonic] (m: Mnemonic) (x: UInt32): InstructionSet.EncArgs m
  := EncType.mkEncArgs x

end RiscV.Instr.Types
