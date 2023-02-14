/-
 Copyright 2023 RISC Zero, Inc.

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
-/

import RiscV.Mach.Exception
import RiscV.Mach.Mem
import RiscV.Mach.Reg

namespace RiscV.Monad

open RiscV.Mach.Exception
open RiscV.Mach.Mem
open RiscV.Mach.Reg


structure MachineState where
  mem: Mem
  reg_file: RegFile

namespace MachineState
  @[always_inline]
  instance [Monad M] [MonadStateOf MachineState M] : MonadStateOf Mem M where
    get
      := do let self <- get
            pure self.mem
    set mem
      := do let self <- get
            set { self with mem }
    modifyGet f
      := do let self <- get
            let (out, mem) := f self.mem
            set { self with mem }
            pure out

  @[always_inline]
  instance [Monad M] [MonadStateOf MachineState M] : MonadStateOf RegFile M where
    get
      := do let self <- get
            pure self.reg_file
    set reg_file
      := do let self <- get
            set { self with reg_file }
    modifyGet f
      := do let self <- get
            let (out, reg_file) := f self.reg_file
            set { self with reg_file }
            pure out
end MachineState


class MonadMachine (M: Type -> Type)
  extends
    Monad M,
    MonadExceptOf RiscVException M,
    MonadStateOf MachineState M
  where

namespace MonadMachine
  @[always_inline, inline]
  def getMachine [MonadMachine M]: M MachineState
    := MonadStateOf.get

  @[always_inline, inline]
  def getReg [MonadMachine M] (reg: Reg): M UInt32
    := RegFile.get_word reg

  @[always_inline, inline]
  def fetchWord [MonadMachine M] (addr: UInt32): M UInt32
    := Mem.get_word addr.toNat

  @[always_inline]
  instance CanonicalInstance [Monad M] [MonadExceptOf RiscVException M] [MonadStateOf MachineState M] : MonadMachine M where

  @[always_inline]
  instance LiftInstance [Monad M] : MonadLift M (ExceptT RiscVException (StateT Machine M)) where
    monadLift f := ExceptT.lift (StateT.lift f)
end MonadMachine


namespace MachineState
  @[always_inline, inline]
  def run [Monad M] (machine: MachineState) (f: {M': Type -> Type} -> [MonadMachine M'] -> [MonadLift M M'] -> M' X): M (Except RiscVException X × MachineState)
    := StateT.run (ExceptT.run f) machine

  @[always_inline, inline]
  def run' [Monad M] (machine: MachineState) (f: {M': Type -> Type} -> [MonadMachine M'] -> [MonadLift M M'] -> M' X): M (Except RiscVException X)
    := StateT.run' (ExceptT.run f) machine
end MachineState

end RiscV.Monad
