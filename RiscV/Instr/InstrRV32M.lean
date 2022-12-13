/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import RiscV.Instr.Types
import RiscV.Int
import RiscV.Monad
import RiscV.Reg

namespace RiscV.Instr.InstrRV32M

open R0sy.Lean.UInt64
open Int
open Monad
open Reg
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

instance : ToString RV32M where
  toString
    | .MUL => "MUL" | .MULH => "MULH" | .MULHSU => "MULHSU" | .MULHU => "MULHU" | .DIV => "DIV" | .DIVU => "DIVU" | .REM => "REM" | .REMU => "REMU"

instance : InstructionSet RV32M where
  all := #[
    .MUL, .MULH, .MULHSU, .MULHU, .DIV, .DIVU, .REM, .REMU
  ]
  encode_mnemonic (m: RV32M)
    := match m with
        | .MUL =>    { type := .R,  mnemonic := R.EncMnemonic.new   0b0000001   0b000   0b0110011 }
        | .MULH =>   { type := .R,  mnemonic := R.EncMnemonic.new   0b0000001   0b001   0b0110011 }
        | .MULHSU => { type := .R,  mnemonic := R.EncMnemonic.new   0b0000001   0b010   0b0110011 }
        | .MULHU =>  { type := .R,  mnemonic := R.EncMnemonic.new   0b0000001   0b011   0b0110011 }
        | .DIV =>    { type := .R,  mnemonic := R.EncMnemonic.new   0b0000001   0b100   0b0110011 }
        | .DIVU =>   { type := .R,  mnemonic := R.EncMnemonic.new   0b0000001   0b101   0b0110011 }
        | .REM =>    { type := .R,  mnemonic := R.EncMnemonic.new   0b0000001   0b110   0b0110011 }
        | .REMU =>   { type := .R,  mnemonic := R.EncMnemonic.new   0b0000001   0b111   0b0110011 }
  run
    | .MUL, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              RegFile.set_word args.rd (x * y)
    | .MULH, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              let z := UInt32.extend_signed x * UInt32.extend_signed y
              RegFile.set_word args.rd (UInt64.hi z)
    | .MULHSU, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              let z := UInt32.extend_signed x * UInt32.extend_unsigned y
              RegFile.set_word args.rd (UInt64.hi z)
    | .MULHU, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              let z := UInt32.extend_unsigned x * UInt32.extend_unsigned y
              RegFile.set_word args.rd (UInt64.hi z)
    | .DIV, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              let q :=
                if x == UInt32.min_signed && y == UInt32.neg_one then x
                else if y == 0 then UInt32.neg_one
                else
                  let sgn_x := if UInt32.is_neg x then UInt32.neg_one else 1
                  let sgn_y := if UInt32.is_neg y then UInt32.neg_one else 1
                  (sgn_x * sgn_y) * ((x * sgn_x) / (y * sgn_y))
              RegFile.set_word args.rd q
    | .DIVU, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              let q :=
                if y == 0 then UInt32.max_unsigned
                else x / y
              RegFile.set_word args.rd q
    | .REM, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              let r :=
                if x == UInt32.min_signed && y == UInt32.neg_one then 0
                else if y == 0 then x
                else
                  let sgn_x := if UInt32.is_neg x then UInt32.neg_one else 1
                  let sgn_y := if UInt32.is_neg y then UInt32.neg_one else 1
                  (sgn_x * sgn_y) * ((x * sgn_x) % (y * sgn_y))
              RegFile.set_word args.rd r
    | .REMU, args
        => do let x <- RegFile.get_word args.rs1
              let y <- RegFile.get_word args.rs2
              let r :=
                if y == 0 then x
                else x % y
              RegFile.set_word args.rd r

end RiscV.Instr.InstrRV32M
