/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV.Mach.Exception
import RiscV.Mach.Mem
import RiscV.Mach.Reg

namespace RiscV.Monad

open RiscV.Mach.Exception
open RiscV.Mach.Mem
open RiscV.Mach.Reg

structure Machine where
  mem: Mem
  reg_file: RegFile

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

class MonadMachine (M: Type -> Type)
  extends
    Monad M,
    MonadExceptOf RiscVException M,
    MonadStateOf Machine M
  where

instance MonadMachine.Instance [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf Machine M] : MonadMachine M where

def MonadMachine.run [Monad M] (machine: Machine) (f: {M': Type -> Type} -> [MonadMachine M'] -> M' X): M (Except RiscVException (X × Machine))
  := do let M' := StateT Machine (ExceptT RiscVException M)
        ExceptT.run (StateT.run (@f M' _) machine)

def MonadMachine.run' [Monad M] (machine: Machine) (f: {M': Type -> Type} -> [MonadMachine M'] -> M' X): M (Except RiscVException X)
  := do let M' := StateT Machine (ExceptT RiscVException M)
        ExceptT.run (StateT.run' (@f M' _) machine)

end RiscV.Monad
