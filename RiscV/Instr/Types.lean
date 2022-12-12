/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import RiscV.Mem
import RiscV.Monad
import RiscV.Reg

namespace RiscV.Instr.Types

open R0sy.Data.Bits
open R0sy.Lean.UInt32
open Mem
open Monad
open Reg


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

  def EncMnemonic.deserialize (x: UInt32): EncMnemonic
    := {
      funct7 := Bits.ofUInt32 x
      funct3 := Bits.ofUInt32 x
      opcode := Bits.ofUInt32 x
    }

  def EncMnemonic.serialize (x: EncMnemonic): UInt32
    := x.funct7.toUInt32 ||| x.funct3.toUInt32 ||| x.opcode.toUInt32


  structure EncArgs where
    rs2: Bits 24 20
    rs1: Bits 19 15
    rd: Bits 11 7

  def EncArgs.deserialize (x: UInt32): EncArgs
    := {
      rs2 := Bits.ofUInt32 x
      rs1 := Bits.ofUInt32 x
      rd := Bits.ofUInt32 x
    }


  structure Args where
    rd: Reg
    rs1: Reg
    rs2: Reg

  def EncArgs.decode (x: EncArgs): Args
    := {
      rd := Reg.ofUInt32 x.rd.val,
      rs1 := Reg.ofUInt32 x.rs1.val,
      rs2 := Reg.ofUInt32 x.rs2.val
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

  def EncMnemonic.deserialize (x: UInt32): EncMnemonic
    := {
      funct3 := Bits.ofUInt32 x
      opcode := Bits.ofUInt32 x
    }

  def EncMnemonic.serialize (x: EncMnemonic): UInt32
    := x.funct3.toUInt32 ||| x.opcode.toUInt32


  structure EncArgs where
    imm11_0: Bits 31 20
    rs1: Bits 19 15
    rd: Bits 11 7

  def EncArgs.deserialize (x: UInt32): EncArgs
    := {
      imm11_0 := Bits.ofUInt32 x
      rs1 := Bits.ofUInt32 x
      rd := Bits.ofUInt32 x
    }


  structure Args where
    rd: Reg
    rs1: Reg
    imm: UInt32

  def EncArgs.decode (x: EncArgs): Args
    := Id.run do
        let imm11_0:  Bits 11 0 := { val := x.imm11_0.val }
        let imm31_12: Bits 31 12 :=
          if UInt32.test_bit 11 imm11_0.toUInt32
          then Bits.ofUInt32 0xffffffff
          else Bits.ofUInt32 0x00000000
        let imm := imm31_12.toUInt32 ||| imm11_0.toUInt32
        pure {
          rd := Reg.ofUInt32 x.rd.val,
          rs1 := Reg.ofUInt32 x.rs1.val,
          imm
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

  def EncMnemonic.deserialize (x: UInt32): EncMnemonic
    := {
      funct3 := Bits.ofUInt32 x
      opcode := Bits.ofUInt32 x
    }

  def EncMnemonic.serialize (x: EncMnemonic): UInt32
    := x.funct3.toUInt32 ||| x.opcode.toUInt32


  structure EncArgs where
    imm11_5: Bits 31 25
    rs2: Bits 24 20
    rs1: Bits 19 15
    imm4_0: Bits 11 7

  def EncArgs.deserialize (x: UInt32): EncArgs
    := {
      imm11_5 := Bits.ofUInt32 x
      rs2 := Bits.ofUInt32 x
      rs1 := Bits.ofUInt32 x
      imm4_0 := Bits.ofUInt32 x
    }


  structure Args where
    rs1: Reg
    rs2: Reg
    imm: UInt32

  def EncArgs.decode (x: EncArgs): Args
    := Id.run do
        let imm4_0:  Bits 11 0 := { val := x.imm4_0.val }
        let imm11_5:  Bits 11 0 := { val := x.imm11_5.val }
        let imm31_12: Bits 31 12 :=
          if UInt32.test_bit 11 imm11_5.toUInt32
          then Bits.ofUInt32 0xffffffff
          else Bits.ofUInt32 0x00000000
        let imm := imm31_12.toUInt32 ||| imm11_5.toUInt32 ||| imm4_0.toUInt32
        pure {
          rs1 := Reg.ofUInt32 x.rs1.val,
          rs2 := Reg.ofUInt32 x.rs2.val,
          imm
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

  def EncMnemonic.deserialize (x: UInt32): EncMnemonic
    := {
      funct3 := Bits.ofUInt32 x
      opcode := Bits.ofUInt32 x
    }

  def EncMnemonic.serialize (x: EncMnemonic): UInt32
    := x.funct3.toUInt32 ||| x.opcode.toUInt32


  structure EncArgs where
    imm12: Bits 31 31
    imm10_5: Bits 30 25
    rs2: Bits 24 20
    rs1: Bits 19 15
    imm4_1: Bits 11 8
    imm11: Bits 7 7

  def EncArgs.deserialize (x: UInt32): EncArgs
    := {
      imm12 := Bits.ofUInt32 x
      imm10_5 := Bits.ofUInt32 x
      rs2 := Bits.ofUInt32 x
      rs1 := Bits.ofUInt32 x
      imm4_1 := Bits.ofUInt32 x
      imm11 := Bits.ofUInt32 x
    }


  structure Args where
    rs1: Reg
    rs2: Reg
    imm: UInt32

  def EncArgs.decode (x: EncArgs): Args
    := Id.run do
        let imm12: Bits 12 12 := { val := x.imm12.val }
        let imm11: Bits 11 11 := { val := x.imm11.val }
        let imm10_5: Bits 10 5 := { val := x.imm10_5.val }
        let imm4_1: Bits 4 1 := { val := x.imm4_1.val }
        let imm0: Bits 0 0 := { val := 0 }
        let imm31_13: Bits 31 13 :=
          if UInt32.test_bit 12 imm12.toUInt32
          then Bits.ofUInt32 0xffffffff
          else Bits.ofUInt32 0x00000000
        let imm := imm31_13.toUInt32 ||| imm12.toUInt32 ||| imm11.toUInt32 ||| imm10_5.toUInt32 ||| imm4_1.toUInt32 ||| imm0.toUInt32
        pure {
          rs1 := Reg.ofUInt32 x.rs1.val,
          rs2 := Reg.ofUInt32 x.rs2.val,
          imm
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

  def EncMnemonic.deserialize (x: UInt32): EncMnemonic
    := {
      opcode := Bits.ofUInt32 x
    }

  def EncMnemonic.serialize (x: EncMnemonic): UInt32
    := x.opcode.toUInt32


  structure EncArgs where
    imm31_12: Bits 31 12
    rd: Bits 11 7

  def EncArgs.deserialize (x: UInt32): EncArgs
    := {
      imm31_12 := Bits.ofUInt32 x
      rd := Bits.ofUInt32 x
    }


  structure Args where
    rd: Reg
    imm: UInt32

  def EncArgs.decode (x: EncArgs): Args
    := Id.run do
        let imm31_12: Bits 31 12 := { val := x.imm31_12.val }
        let imm11_13: Bits 11 0 := { val := 0 }
        let imm := imm31_12.toUInt32 ||| imm11_13.toUInt32
        pure {
          rd := Reg.ofUInt32 x.rd.val,
          imm
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

  def EncMnemonic.deserialize (x: UInt32): EncMnemonic
    := {
      opcode := Bits.ofUInt32 x
    }

  def EncMnemonic.serialize (x: EncMnemonic): UInt32
    := x.opcode.toUInt32


  structure EncArgs where
    imm20: Bits 31 31
    imm10_1: Bits 30 21
    imm11: Bits 20 20
    imm19_12: Bits 19 12
    rd: Bits 11 7

  def EncArgs.deserialize (x: UInt32): EncArgs
    := {
      imm20 := Bits.ofUInt32 x
      imm10_1 := Bits.ofUInt32 x
      imm11 := Bits.ofUInt32 x
      imm19_12 := Bits.ofUInt32 x
      rd := Bits.ofUInt32 x
    }


  structure Args where
    rd: Reg
    imm: UInt32

  def EncArgs.decode (x: EncArgs): Args
    := Id.run do
        let imm20: Bits 20 20 := { val := x.imm20.val }
        let imm19_12: Bits 19 12 := { val := x.imm19_12.val }
        let imm11: Bits 11 11 := { val := x.imm11.val }
        let imm10_1: Bits 10 1 := { val := x.imm10_1.val }
        let imm0: Bits 0 0 := { val := 0 }
        let imm := imm20.toUInt32 ||| imm19_12.toUInt32 ||| imm11.toUInt32 ||| imm10_1.toUInt32 ||| imm0.toUInt32
        pure {
          rd := Reg.ofUInt32 x.rd.val,
          imm
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

  def EncMnemonic.deserialize (x: UInt32): EncMnemonic
    := {
      const31_20 := Bits.ofUInt32 x
      const19_15 := Bits.ofUInt32 x
      const14_12 := Bits.ofUInt32 x
      const11_7 := Bits.ofUInt32 x
      opcode := Bits.ofUInt32 x
    }

  def EncMnemonic.serialize (x: EncMnemonic): UInt32
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

namespace EncType
  def EncMnemonic (t: EncType): Type
    := match t with
        | R => R.EncMnemonic
        | I => I.EncMnemonic
        | S => S.EncMnemonic
        | B => B.EncMnemonic
        | U => U.EncMnemonic
        | J => J.EncMnemonic
        | Const => Const.EncMnemonic

  def EncMnemonic.mask (t: EncType): UInt32
    := match t with
        | R => R.EncMnemonic.mask
        | I => I.EncMnemonic.mask
        | S => S.EncMnemonic.mask
        | B => B.EncMnemonic.mask
        | U => U.EncMnemonic.mask
        | J => J.EncMnemonic.mask
        | Const => Const.EncMnemonic.mask

  def EncMnemonic.deserialize {t: EncType} (x: UInt32): EncType.EncMnemonic t
    := match t with
        | R => R.EncMnemonic.deserialize x
        | I => I.EncMnemonic.deserialize x
        | S => S.EncMnemonic.deserialize x
        | B => B.EncMnemonic.deserialize x
        | U => U.EncMnemonic.deserialize x
        | J => J.EncMnemonic.deserialize x
        | Const => Const.EncMnemonic.deserialize x

  def EncMnemonic.serialize {t: EncType} (code: EncType.EncMnemonic t): UInt32
    := match t with
        | R => R.EncMnemonic.serialize code
        | I => I.EncMnemonic.serialize code
        | S => S.EncMnemonic.serialize code
        | B => B.EncMnemonic.serialize code
        | U => U.EncMnemonic.serialize code
        | J => J.EncMnemonic.serialize code
        | Const => Const.EncMnemonic.serialize code

  def EncArgs (t: EncType): Type
    := match t with
        | R => R.EncArgs
        | I => I.EncArgs
        | S => S.EncArgs
        | B => B.EncArgs
        | U => U.EncArgs
        | J => J.EncArgs
        | Const => Unit

  def EncArgs.deserialize: {t: EncType} -> UInt32 -> EncType.EncArgs t
    | R, x => R.EncArgs.deserialize x
    | I, x => I.EncArgs.deserialize x
    | S, x => S.EncArgs.deserialize x
    | B, x => B.EncArgs.deserialize x
    | U, x => U.EncArgs.deserialize x
    | J, x => J.EncArgs.deserialize x
    | Const, _ => ()

  def Args (t: EncType): Type
    := match t with
        | R => R.Args
        | I => I.Args
        | S => S.Args
        | B => B.Args
        | U => U.Args
        | J => J.Args
        | Const => Unit

  def EncArgs.decode: {t: EncType} -> EncType.EncArgs t -> EncType.Args t
    | R, x => R.EncArgs.decode x
    | I, x => I.EncArgs.decode x
    | S, x => S.EncArgs.decode x
    | B, x => B.EncArgs.decode x
    | U, x => U.EncArgs.decode x
    | J, x => J.EncArgs.decode x
    | Const, _ => ()
end EncType


structure BoxedEncMnemonic where
  type: EncType
  mnemonic: EncType.EncMnemonic type


class InstructionSet (Mnemonic: Type) where
  all: Array Mnemonic
  encode_mnemonic (m: Mnemonic): BoxedEncMnemonic
  run [MonadMachine M] (m: Mnemonic) (args: EncType.Args (encode_mnemonic m).type): M Unit

namespace InstructionSet
  def EncMnemonic.serialize [InstructionSet Mnemonic] (m: Mnemonic): UInt32
    := EncType.EncMnemonic.serialize (InstructionSet.encode_mnemonic m).mnemonic

  def code_matches [InstructionSet Mnemonic] (m: Mnemonic) (x: UInt32): Bool
    := let mask := EncType.EncMnemonic.mask (InstructionSet.encode_mnemonic m).type
      x &&& mask == InstructionSet.EncMnemonic.serialize m

  def deserialize_mnemonic (Mnemonic: Type) [InstructionSet Mnemonic] (x: UInt32): Option Mnemonic
    := Id.run do
        for mnemonic in @InstructionSet.all Mnemonic _ do
          if InstructionSet.code_matches mnemonic x then return (some mnemonic)
        pure none

  def EncArgs [InstructionSet Mnemonic] (m: Mnemonic): Type
    := EncType.EncArgs (InstructionSet.encode_mnemonic m).type

  def EncArgs.deserialize [InstructionSet Mnemonic] (m: Mnemonic) (x: UInt32): InstructionSet.EncArgs m
    := EncType.EncArgs.deserialize x

  def Args [InstructionSet Mnemonic] (m: Mnemonic): Type
    := EncType.Args (InstructionSet.encode_mnemonic m).type

  def EncArgs.decode [InstructionSet Mnemonic] (m: Mnemonic) (x: InstructionSet.EncArgs m): InstructionSet.Args m
    := EncType.EncArgs.decode x

  def step (Mnemonic: Type) [MonadMachine M] [InstructionSet Mnemonic]: M Unit
    := do let pc <- RegFile.get_word .PC
          let instr <- Mem.get_word { val := pc }
          match deserialize_mnemonic Mnemonic instr with
            | none => throw (.InvalidInstruction pc instr)
            | some mnemonic
                => do let enc_args := EncArgs.deserialize mnemonic instr
                      let args := EncArgs.decode mnemonic enc_args
                      InstructionSet.run mnemonic args
end InstructionSet

end RiscV.Instr.Types
