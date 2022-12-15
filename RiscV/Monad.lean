/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV.Instr.Types
import RiscV.Mach.Exception
import RiscV.Mach.Mem
import RiscV.Mach.Reg

namespace RiscV.Monad

open RiscV.Instr.Types
open RiscV.Mach.Exception
open RiscV.Mach.Mem
open RiscV.Mach.Reg


structure Machine where
  mem: Mem
  reg_file: RegFile

namespace Machine
  instance [Monad M] [MonadStateOf Machine M] : MonadStateOf Mem M where
    get
      := do let self <- get
            pure self.mem
    set mem
      := do let self <- get
            set { self with mem}
    modifyGet f
      := do let self <- get
            let (out, mem) := f self.mem
            set { self with mem }
            pure out

  instance [Monad M] [MonadStateOf Machine M] : MonadStateOf RegFile M where
    get
      := do let self <- get
            pure self.reg_file
    set reg_file
      := do let self <- get
            set { self with reg_file}
    modifyGet f
      := do let self <- get
            let (out, reg_file) := f self.reg_file
            set { self with reg_file }
            pure out
end Machine


class MonadMachine (M: Type -> Type)
  extends
    Monad M,
    MonadExceptOf RiscVException M,
    MonadStateOf Machine M
  where

namespace MonadMachine
  instance CanonicalInstance [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Machine M] : MonadMachine M where

  instance LiftInstance [Monad M] : MonadLift M (StateT Machine (ExceptT RiscVException M)) where
    monadLift f := StateT.lift (ExceptT.lift f)

  def run [Monad M] (machine: Machine) (f: {M': Type -> Type} -> [MonadMachine M'] -> [MonadLift M M'] -> M' X): M (Except RiscVException (X × Machine))
    := ExceptT.run (StateT.run f machine)

  def run' [Monad M] (machine: Machine) (f: {M': Type -> Type} -> [MonadMachine M'] -> [MonadLift M M'] -> M' X): M (Except RiscVException X)
    := ExceptT.run (StateT.run' f machine)
end MonadMachine


structure ISA where
  Mnemonic: Type
  all: Array Mnemonic
  toString: Mnemonic -> String
  encode_mnemonic (m: Mnemonic): EncMnemonic
  run [MonadMachine M] (m: Mnemonic) (args: EncMnemonic.Args (encode_mnemonic m)): M Unit

namespace ISA
  def serialize_mnemonic (isa: ISA) (m: isa.Mnemonic): UInt32
    := (isa.encode_mnemonic m).serialize_mnemonic

  def code_matches (isa: ISA) (m: isa.Mnemonic) (x: UInt32): Bool
    := let mask := (isa.encode_mnemonic m).mask_mnemonic
      x &&& mask == isa.serialize_mnemonic m

  def deserialize_mnemonic (isa: ISA) (x: UInt32): Option isa.Mnemonic
    := Id.run do
        for mnemonic in isa.all do
          if isa.code_matches mnemonic x then return (some mnemonic)
        pure none

  def EncArgs (isa: ISA) (m: isa.Mnemonic): Type
    := (isa.encode_mnemonic m).EncArgs

  def deserialize_args (isa: ISA) (m: isa.Mnemonic) (x: UInt32): isa.EncArgs m
    := (isa.encode_mnemonic m).deserialize_args x

  def Args (isa: ISA) (m: isa.Mnemonic): Type
    := (isa.encode_mnemonic m).Args

  instance : ToString (Args isa m) where
    toString x := EncMnemonic.Args.ToString.toString x

  def decode_args (isa: ISA) (m: isa.Mnemonic) (x: isa.EncArgs m): isa.Args m
    := EncMnemonic.decode_args x

  def decode_to_string (isa: ISA) (instr: UInt32): Option String
    := Id.run do
          match isa.deserialize_mnemonic instr with
            | none => pure none
            | some mnemonic
                => do let enc_args := isa.deserialize_args mnemonic instr
                      let args := isa.decode_args mnemonic enc_args
                      pure (some s!"{isa.toString mnemonic}  {args}")

  def step (isa: ISA) [MonadMachine M]: M Unit
    := do let pc <- RegFile.get_word .PC
          let instr <- Mem.get_word { val := pc }
          match isa.deserialize_mnemonic instr with
            | none => throw (.InvalidInstruction pc instr)
            | some mnemonic
                => do RegFile.set_word .PC (pc + 4)
                      let enc_args := isa.deserialize_args mnemonic instr
                      let args := isa.decode_args mnemonic enc_args
                      isa.run mnemonic args
end ISA

end RiscV.Monad
