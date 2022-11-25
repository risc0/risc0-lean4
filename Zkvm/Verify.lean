/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Hash.Sha2
import Zkvm.Circuit
import Zkvm.Verify.Adapter
import Zkvm.Verify.Classes
import Zkvm.Verify.ReadIop

namespace Zkvm.Verify

open R0sy.Hash.Sha2
open Adapter
open Circuit
open Classes
open ReadIop

structure VerifyContext (C: Type) where
  adapter: VerifyAdapter C
  read_iop: ReadIop

instance [Monad M] [MonadStateOf (VerifyContext C) M] : MonadStateOf ReadIop M where
  get
    := do let self <- get
          return self.read_iop
  set read_iop
    := do let self <- get
          set { self with read_iop }
  modifyGet f
    := do let self <- get
          let (result, read_iop) := f self.read_iop
          set { self with read_iop }
          return result

instance [Monad M] [MonadStateOf (VerifyContext C) M] : MonadStateOf (VerifyAdapter C) M where
  get
    := do let self <- get
          return self.adapter
  set adapter
    := do let self <- get
          set { self with adapter }
  modifyGet f
    := do let self <- get
          let (result, adapter) := f self.adapter
          set { self with adapter }
          return result

inductive VerificationError where
  | ReceiptFormatError
  | MethodCycleError (required: UInt64)
  | MethodVerificationError
  | MerkleQueryOutOfRange (idx: UInt64) (rows: UInt64)
  | InvalidProof

class VerifyHal (H: Type) where

def CheckCode: Type := UInt32 -> Sha256.Digest -> Except VerificationError Unit

def do_verify (C: Type) [Monad M] [MonadExceptOf VerificationError M] [MonadStateOf (VerifyContext C) M] [CircuitInfo C] [TapsProvider C] [VerifyHal H]
  (hal: H) (check_code: CheckCode)
  : M Unit
  := do let taps <- MonadVerifyAdapter.getTaps
        return ()

def verify [VerifyHal H] [CircuitInfo C] [TapsProvider C]
  (hal: H) (circuit: C) (seal: Subarray UInt32) (check_code: CheckCode)
  : Id (Except VerificationError Unit)
  := do let verify_context: VerifyContext C := {
          adapter := VerifyAdapter.new circuit,
          read_iop := ReadIop.new seal
        }
        ExceptT.run (StateT.run' (do_verify C hal check_code) verify_context)

end Zkvm.Verify
