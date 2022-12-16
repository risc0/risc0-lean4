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


inductive Variant where
  | RV32IMle

structure Machine (variant: Variant) where
  mem: Mem
  reg_file: RegFile

namespace Machine
  def new_RV32IMle (pc: UInt32) (mem: Mem): Machine .RV32IMle
    := Id.run do
        let (_, reg_file) <- StateT.run (RegFile.set_word .PC pc) RegFile.new
        pure {
          mem,
          reg_file
        }

  instance [Monad M] [MonadStateOf (Machine variant) M] : MonadStateOf Mem M where
    get
      := do let self <- get
            pure self.mem
    set mem
      := do let self <- get
            set ({ self with mem }: Machine variant)
    modifyGet f
      := do let self <- get
            let (out, mem) := f self.mem
            set ({ self with mem }: Machine variant)
            pure out

  instance [Monad M] [MonadStateOf (Machine variant) M] : MonadStateOf RegFile M where
    get
      := do let self <- get
            pure self.reg_file
    set reg_file
      := do let self <- get
            set ({ self with reg_file }: Machine variant)
    modifyGet f
      := do let self <- get
            let (out, reg_file) := f self.reg_file
            set ({ self with reg_file }: Machine variant)
            pure out
end Machine


class MonadMachine (variant: outParam Variant) (M: Type -> Type)
  extends
    Monad M,
    MonadExceptOf RiscVException M,
    MonadStateOf (Machine variant) M
  where

namespace MonadMachine
  def getVariant [MonadMachine variant M]: M Variant
    := pure variant

  def getMachine [MonadMachine variant M]: M (Machine variant)
    := MonadStateOf.get

  def getReg [MonadMachine variant M] (reg: Reg): M UInt32
    := RegFile.get_word reg

  def fetchWord [MonadMachine variant M] (addr: UInt32): M UInt32
    := Mem.get_word { val := addr }

  instance CanonicalInstance [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf (Machine variant) M] : MonadMachine variant M where

  instance LiftInstance [Monad M] : MonadLift M (ExceptT RiscVException (StateT (Machine variant) M)) where
    monadLift f := ExceptT.lift (StateT.lift f)
end MonadMachine


namespace Machine
  def run [Monad M] (machine: Machine variant) (f: {M': Type -> Type} -> [MonadMachine variant M'] -> [MonadLift M M'] -> M' X): M (Except RiscVException X × Machine variant)
    := StateT.run (ExceptT.run f) machine

  def run' [Monad M] (machine: Machine variant) (f: {M': Type -> Type} -> [MonadMachine variant M'] -> [MonadLift M M'] -> M' X): M (Except RiscVException X)
    := StateT.run' (ExceptT.run f) machine
end Machine


structure ISA where
  Mnemonic: Type
  all: Array Mnemonic
  toString: Mnemonic -> String
  encode_mnemonic (m: Mnemonic): EncMnemonic
  run (variant: Variant) [MonadMachine variant M] (m: Mnemonic) (args: EncMnemonic.Args (encode_mnemonic m)): M Unit

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
end ISA

end RiscV.Monad
