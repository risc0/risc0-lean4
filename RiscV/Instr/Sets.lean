/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV.Instr.Types
import RiscV.Mach.Mem
import RiscV.Mach.Reg
import RiscV.Monad

namespace RiscV.Instr.Sets

open RiscV.Instr.Types
open RiscV.Mach.Mem
open RiscV.Mach.Reg
open RiscV.Monad

structure BoxedEncMnemonic where
  type: EncType
  mnemonic: EncType.EncMnemonic type

class InstructionSet (Mnemonic: Type)
  extends
    ToString Mnemonic
  where
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

  instance [H: InstructionSet Mnemonic] : ToString (@Args _ H m) where
    toString x := EncType.Args.ToString.toString x

  def EncArgs.decode [InstructionSet Mnemonic] (m: Mnemonic) (x: InstructionSet.EncArgs m): InstructionSet.Args m
    := EncType.EncArgs.decode x

  def decode_to_string (Mnemonic: Type) [InstructionSet Mnemonic] (instr: UInt32): Option String
    := Id.run do
          match deserialize_mnemonic Mnemonic instr with
            | none => pure none
            | some mnemonic
                => do let enc_args := EncArgs.deserialize mnemonic instr
                      let args := EncArgs.decode mnemonic enc_args
                      pure (some s!"{mnemonic}  {args}")

  def step (Mnemonic: Type) [MonadMachine M] [InstructionSet Mnemonic]: M Unit
    := do let pc <- RegFile.get_word .PC
          let instr <- Mem.get_word { val := pc }
          match deserialize_mnemonic Mnemonic instr with
            | none => throw (.InvalidInstruction pc instr)
            | some mnemonic
                => do RegFile.set_word .PC (pc + 4)
                      let enc_args := EncArgs.deserialize mnemonic instr
                      let args := EncArgs.decode mnemonic enc_args
                      InstructionSet.run mnemonic args
end InstructionSet

end RiscV.Instr.Sets