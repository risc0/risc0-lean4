/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.ArithVM.Circuit
import Zkvm.ArithVM.Taps
import Zkvm.Verify.Classes
import Zkvm.Verify.Merkle
import Zkvm.MethodId
import Zkvm.Verify.ReadIop

namespace Zkvm.Verify.Monad

open R0sy.Algebra
open R0sy.Hash.Sha2
open ArithVM.Circuit
open ArithVM.Taps
open Classes
open Merkle
open MethodId
open ReadIop


structure VerifyContext where
  circuit: Circuit
  method_id: MethodId
  read_iop: ReadIop

class MonadVerify (M: Type -> Type)
  extends
    Monad M,
    MonadStateOf VerifyContext M,
    MonadExceptOf VerificationError M
  where

instance
  [Monad M]
  [MonadStateOf VerifyContext M]
  [MonadExceptOf VerificationError M]
  : MonadVerify M where

instance [Monad M] [MonadStateOf VerifyContext M] : MonadCircuit M where
  getCircuit
    := do let self <- get
          pure self.circuit

instance [Monad M] [MonadStateOf VerifyContext M] : MonadMethodId M where
  getMethodId
    := do let self <- get
          pure self.method_id

instance [Monad M] [MonadStateOf VerifyContext M] : MonadStateOf ReadIop M where
  get
    := do let self <- get
          pure self.read_iop
  set read_iop
    := do let self <- get
          set { self with read_iop }
  modifyGet f
    := do let self <- get
          let (result, read_iop) := f self.read_iop
          set { self with read_iop }
          pure result

def VerifyContext.run (Elem ExtElem: Type) [Algebraic Elem ExtElem] (circuit: Circuit) (method_id: MethodId) (seal: Array UInt32)
  (f: {M: Type -> Type} -> [Monad M] -> [MonadVerify M] -> [Algebraic Elem ExtElem] -> M Unit)
  : Except VerificationError Unit
  := Id.run do
      let verify_context: VerifyContext := {
        circuit,
        method_id,
        read_iop := ReadIop.new seal.toSubarray,
      }
      let M := StateT VerifyContext (ExceptT VerificationError Id)
      ExceptT.run (StateT.run' (@f M _ _ _) verify_context)

end Zkvm.Verify.Monad
