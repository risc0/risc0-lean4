/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV.Register

namespace RiscV.Instruction

open Register

inductive IInstruction where
  | Lb (rd: Register) (rs1: Register) (oimm12: UInt32)
  | Lh (rd: Register) (rs1: Register) (oimm12: UInt32)
  | Lw (rd: Register) (rs1: Register) (oimm12: UInt32)
  | Lbu (rd: Register) (rs1: Register) (oimm12: UInt32)
  | Lhu (rd: Register) (rs1: Register) (oimm12: UInt32)

  | Fence (pred: UInt32) (succ: UInt32)
  | Fence_i

  | Addi (rd: Register) (rs1: Register) (imm12: UInt32)
  | Slli (rd: Register) (rs1: Register) (shamt6: Nat)
  | Slti (rd: Register) (rs1: Register) (imm12: UInt32)
  | Sltiu (rd: Register) (rs1: Register) (imm12: UInt32)
  | Xori (rd: Register) (rs1: Register) (imm12: UInt32)
  | Ori (rd: Register) (rs1: Register) (imm12: UInt32)
  | Andi (rd: Register) (rs1: Register) (imm12: UInt32)
  | Srli (rd: Register) (rs1: Register) (shamt6: Nat)
  | Srai (rd: Register) (rs1: Register) (shamt6: Nat)

  | Auipc (rd: Register) (oimm20: UInt32)

  | Sb (rs1: Register) (rs2: Register) (simm12: UInt32)
  | Sh (rs1: Register) (rs2: Register) (simm12: UInt32)
  | Sw (rs1: Register) (rs2: Register) (simm12: UInt32)

  | Add (rd: Register) (rs1: Register) (rs2: Register)
  | Sub (rd: Register) (rs1: Register) (rs2: Register)
  | Sll (rd: Register) (rs1: Register) (rs2: Register)
  | Slt (rd: Register) (rs1: Register) (rs2: Register)
  | Sltu (rd: Register) (rs1: Register) (rs2: Register)
  | Xor (rd: Register) (rs1: Register) (rs2: Register)
  | Srl (rd: Register) (rs1: Register) (rs2: Register)
  | Sra (rd: Register) (rs1: Register) (rs2: Register)
  | Or (rd: Register) (rs1: Register) (rs2: Register)
  | And (rd: Register) (rs1: Register) (rs2: Register)

  | Lui (rd: Register) (imm20: UInt32)

  | Beq (rs1: Register) (rs2: Register) (sbimm12: UInt32)
  | Bne (rs1: Register) (rs2: Register) (sbimm12: UInt32)
  | Blt (rs1: Register) (rs2: Register) (sbimm12: UInt32)
  | Bge (rs1: Register) (rs2: Register) (sbimm12: UInt32)
  | Bltu (rs1: Register) (rs2: Register) (sbimm12: UInt32)
  | Bgeu (rs1: Register) (rs2: Register) (sbimm12: UInt32)

  | Jalr (rd: Register) (rs1: Register) (oimm12: UInt32)
  | Jal (rd: Register) (jimm20: UInt32)

  | InvalidI


inductive MInstruction where
  | Mul (rd: Register) (rs1: Register) (rs2: Register)
  | Mulh (rd: Register) (rs1: Register) (rs2: Register)
  | Mulhsu (rd: Register) (rs1: Register) (rs2: Register)
  | Mulhu (rd: Register) (rs1: Register) (rs2: Register)
  | Div (rd: Register) (rs1: Register) (rs2: Register)
  | Divu (rd: Register) (rs1: Register) (rs2: Register)
  | Rem (rd: Register) (rs1: Register) (rs2: Register)
  | Remu (rd: Register) (rs1: Register) (rs2: Register)
  | InvalidM

inductive Instruction where
  | I (instr: IInstruction)
  | M (instr: MInstruction)

end RiscV.Instruction
