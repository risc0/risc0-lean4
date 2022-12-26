/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import RiscV.Mach.Reg

namespace RiscV.Instr.Types

open R0sy.Data.Bits
open R0sy.Data.Hex
open R0sy.Lean.UInt32
open RiscV.Mach.Reg


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

  @[always_inline, inline]
  def EncMnemonic.new (funct7 funct3 opcode: UInt32): EncMnemonic
    := {
      funct7 := { val := funct7 }
      funct3 := { val := funct3 }
      opcode := { val := opcode }
    }

  @[always_inline, inline]
  def EncMnemonic.mask: UInt32 := Masks.funct7_mask ||| Masks.funct3_mask ||| Masks.opcode_mask

  @[always_inline, inline]
  def EncMnemonic.deserialize (x: UInt32): EncMnemonic
    := {
      funct7 := Bits.ofUInt32 x
      funct3 := Bits.ofUInt32 x
      opcode := Bits.ofUInt32 x
    }

  @[always_inline, inline]
  def EncMnemonic.serialize (x: EncMnemonic): UInt32
    := x.funct7.toUInt32 ||| x.funct3.toUInt32 ||| x.opcode.toUInt32


  structure EncArgs where
    rs2: Bits 24 20
    rs1: Bits 19 15
    rd: Bits 11 7

  @[always_inline, inline]
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

  instance : ToString Args where
    toString args := s!"rd:{args.rd}  rs1:{args.rs1}  rs2:{args.rs2}"

  @[always_inline, inline]
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

  @[always_inline, inline]
  def EncMnemonic.new (funct3 opcode: UInt32): EncMnemonic
    := {
      funct3 := { val := funct3 }
      opcode := { val := opcode }
    }

  @[always_inline, inline]
  def EncMnemonic.mask: UInt32 := Masks.funct3_mask ||| Masks.opcode_mask

  @[always_inline, inline]
  def EncMnemonic.deserialize (x: UInt32): EncMnemonic
    := {
      funct3 := Bits.ofUInt32 x
      opcode := Bits.ofUInt32 x
    }

  @[always_inline, inline]
  def EncMnemonic.serialize (x: EncMnemonic): UInt32
    := x.funct3.toUInt32 ||| x.opcode.toUInt32


  structure EncArgs where
    imm11_0: Bits 31 20
    rs1: Bits 19 15
    rd: Bits 11 7

  @[always_inline, inline]
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

  instance : ToString Args where
    toString args := s!"rd:{args.rd}  rs1:{args.rs1}  imm:{UInt32.toHex args.imm}"

  @[always_inline, inline]
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

  @[always_inline, inline]
  def EncMnemonic.new (funct3 opcode: UInt32): EncMnemonic
    := {
      funct3 := { val := funct3 }
      opcode := { val := opcode }
    }

  @[always_inline, inline]
  def EncMnemonic.mask: UInt32 := Masks.funct3_mask ||| Masks.opcode_mask

  @[always_inline, inline]
  def EncMnemonic.deserialize (x: UInt32): EncMnemonic
    := {
      funct3 := Bits.ofUInt32 x
      opcode := Bits.ofUInt32 x
    }

  @[always_inline, inline]
  def EncMnemonic.serialize (x: EncMnemonic): UInt32
    := x.funct3.toUInt32 ||| x.opcode.toUInt32


  structure EncArgs where
    imm11_5: Bits 31 25
    rs2: Bits 24 20
    rs1: Bits 19 15
    imm4_0: Bits 11 7

  @[always_inline, inline]
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

  instance : ToString Args where
    toString args := s!"rs1:{args.rs1}  rs2:{args.rs2}  imm:{UInt32.toHex args.imm}"

  @[always_inline, inline]
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

  @[always_inline, inline]
  def EncMnemonic.new (funct3 opcode: UInt32): EncMnemonic
    := {
      funct3 := { val := funct3 }
      opcode := { val := opcode }
    }

  @[always_inline, inline]
  def EncMnemonic.mask: UInt32 := Masks.funct3_mask ||| Masks.opcode_mask

  @[always_inline, inline]
  def EncMnemonic.deserialize (x: UInt32): EncMnemonic
    := {
      funct3 := Bits.ofUInt32 x
      opcode := Bits.ofUInt32 x
    }

  @[always_inline, inline]
  def EncMnemonic.serialize (x: EncMnemonic): UInt32
    := x.funct3.toUInt32 ||| x.opcode.toUInt32


  structure EncArgs where
    imm12: Bits 31 31
    imm10_5: Bits 30 25
    rs2: Bits 24 20
    rs1: Bits 19 15
    imm4_1: Bits 11 8
    imm11: Bits 7 7

  @[always_inline, inline]
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

  instance : ToString Args where
    toString args := s!"rs1:{args.rs1}  rs2:{args.rs2}  imm:{UInt32.toHex args.imm}"

  @[always_inline, inline]
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

  @[always_inline, inline]
  def EncMnemonic.new (opcode: UInt32): EncMnemonic
    := {
      opcode := { val := opcode }
    }

  @[always_inline, inline]
  def EncMnemonic.mask: UInt32 := Masks.opcode_mask

  @[always_inline, inline]
  def EncMnemonic.deserialize (x: UInt32): EncMnemonic
    := {
      opcode := Bits.ofUInt32 x
    }

  @[always_inline, inline]
  def EncMnemonic.serialize (x: EncMnemonic): UInt32
    := x.opcode.toUInt32


  structure EncArgs where
    imm31_12: Bits 31 12
    rd: Bits 11 7

  @[always_inline, inline]
  def EncArgs.deserialize (x: UInt32): EncArgs
    := {
      imm31_12 := Bits.ofUInt32 x
      rd := Bits.ofUInt32 x
    }


  structure Args where
    rd: Reg
    imm: UInt32

  instance : ToString Args where
    toString args := s!"rd:{args.rd}  imm:{UInt32.toHex args.imm}"

  @[always_inline, inline]
  def EncArgs.decode (x: EncArgs): Args
    := Id.run do
        let imm31_12: Bits 31 12 := { val := x.imm31_12.val }
        let imm := imm31_12.toUInt32
        pure {
          rd := Reg.ofUInt32 x.rd.val,
          imm
        }
end U


namespace J
  structure EncMnemonic where
    opcode: Bits 6 0

  @[always_inline, inline]
  def EncMnemonic.new (opcode: UInt32): EncMnemonic
    := {
      opcode := { val := opcode }
    }

  @[always_inline, inline]
  def EncMnemonic.mask: UInt32 := Masks.opcode_mask

  @[always_inline, inline]
  def EncMnemonic.deserialize (x: UInt32): EncMnemonic
    := {
      opcode := Bits.ofUInt32 x
    }

  @[always_inline, inline]
  def EncMnemonic.serialize (x: EncMnemonic): UInt32
    := x.opcode.toUInt32


  structure EncArgs where
    imm20: Bits 31 31
    imm10_1: Bits 30 21
    imm11: Bits 20 20
    imm19_12: Bits 19 12
    rd: Bits 11 7

  @[always_inline, inline]
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

  instance : ToString Args where
    toString args := s!"rd:{args.rd}  imm:{UInt32.toHex args.imm}"

  @[always_inline, inline]
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

  @[always_inline, inline]
  def EncMnemonic.new (const31_20 const19_15 const14_12 const11_7 opcode: UInt32): EncMnemonic
    := {
      const31_20 := { val := const31_20 },
      const19_15 := { val := const19_15 },
      const14_12 := { val := const14_12 },
      const11_7 := { val := const11_7 },
      opcode := { val := opcode }
    }

  @[always_inline, inline]
  def EncMnemonic.mask: UInt32 := Bits.mask 31 0

  @[always_inline, inline]
  def EncMnemonic.deserialize (x: UInt32): EncMnemonic
    := {
      const31_20 := Bits.ofUInt32 x
      const19_15 := Bits.ofUInt32 x
      const14_12 := Bits.ofUInt32 x
      const11_7 := Bits.ofUInt32 x
      opcode := Bits.ofUInt32 x
    }

  @[always_inline, inline]
  def EncMnemonic.serialize (x: EncMnemonic): UInt32
    := x.const31_20.toUInt32 ||| x.const19_15.toUInt32 ||| x.const14_12.toUInt32 ||| x.const11_7.toUInt32 ||| x.opcode.toUInt32
end Const


inductive EncMnemonic where
  | R (enc: R.EncMnemonic)
  | I (enc: I.EncMnemonic)
  | S (enc: S.EncMnemonic)
  | B (enc: B.EncMnemonic)
  | U (enc: U.EncMnemonic)
  | J (enc: J.EncMnemonic)
  | Const (enc: Const.EncMnemonic)

namespace EncMnemonic
  @[always_inline, inline]
  def mask_mnemonic (t: EncMnemonic): UInt32
    := match t with
        | .R _ => R.EncMnemonic.mask
        | .I _ => I.EncMnemonic.mask
        | .S _ => S.EncMnemonic.mask
        | .B _ => B.EncMnemonic.mask
        | .U _ => U.EncMnemonic.mask
        | .J _ => J.EncMnemonic.mask
        | .Const _ => Const.EncMnemonic.mask

  @[always_inline, inline]
  def serialize_mnemonic (t: EncMnemonic): UInt32
    := match t with
        | .R code => R.EncMnemonic.serialize code
        | .I code => I.EncMnemonic.serialize code
        | .S code => S.EncMnemonic.serialize code
        | .B code => B.EncMnemonic.serialize code
        | .U code => U.EncMnemonic.serialize code
        | .J code => J.EncMnemonic.serialize code
        | .Const code => Const.EncMnemonic.serialize code

  def EncArgs (t: EncMnemonic): Type
    := match t with
        | .R _ => R.EncArgs
        | .I _ => I.EncArgs
        | .S _ => S.EncArgs
        | .B _ => B.EncArgs
        | .U _ => U.EncArgs
        | .J _ => J.EncArgs
        | .Const _ => Unit

  @[always_inline, inline]
  def deserialize_args: (t: EncMnemonic) -> UInt32 -> EncMnemonic.EncArgs t
    | .R _, x => R.EncArgs.deserialize x
    | .I _, x => I.EncArgs.deserialize x
    | .S _, x => S.EncArgs.deserialize x
    | .B _, x => B.EncArgs.deserialize x
    | .U _, x => U.EncArgs.deserialize x
    | .J _, x => J.EncArgs.deserialize x
    | .Const _, _ => ()

  def Args (t: EncMnemonic): Type
    := match t with
        | .R _ => R.Args
        | .I _ => I.Args
        | .S _ => S.Args
        | .B _ => B.Args
        | .U _ => U.Args
        | .J _ => J.Args
        | .Const _ => Unit

  instance Args.ToString : ToString (Args t) where
    toString
      := match t with
          | .R _ => fun (x: R.Args) => ToString.toString x
          | .I _ => fun (x: I.Args) => ToString.toString x
          | .S _ => fun (x: S.Args) => ToString.toString x
          | .B _ => fun (x: B.Args) => ToString.toString x
          | .U _ => fun (x: U.Args) => ToString.toString x
          | .J _ => fun (x: J.Args) => ToString.toString x
          | .Const _ => fun _ => ""

  @[always_inline, inline]
  def decode_args: {t: EncMnemonic} -> EncArgs t -> Args t
    | .R _, x => R.EncArgs.decode x
    | .I _, x => I.EncArgs.decode x
    | .S _, x => S.EncArgs.decode x
    | .B _, x => B.EncArgs.decode x
    | .U _, x => U.EncArgs.decode x
    | .J _, x => J.EncArgs.decode x
    | .Const _, _ => ()
end EncMnemonic

end RiscV.Instr.Types
