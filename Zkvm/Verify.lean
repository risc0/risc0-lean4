/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Hash.Sha2
import Zkvm.Circuit
import Zkvm.Taps
import Zkvm.Verify.Adapter
import Zkvm.Verify.Classes
import Zkvm.Verify.Fri
import Zkvm.Verify.Hal
import Zkvm.Verify.Merkle
import Zkvm.Verify.ReadIop

namespace Zkvm.Verify

open R0sy.Hash.Sha2
open Adapter
open Circuit
open Classes
open Fri
open Hal
open Merkle
open ReadIop
open Taps

inductive VerificationError where
  | ReceiptFormatError
  | MethodCycleError (required: UInt64)
  | MethodVerificationError
  | MerkleQueryOutOfRange (idx: UInt64) (rows: UInt64)
  | InvalidProof

def CheckCode
  := (M: Type -> Type)
  -> [Monad M]
  -> [MonadExceptOf VerificationError M]
  -> UInt32
  -> Sha256.Digest
  -> M Unit


structure VerifyContext (Elem ExtElem C: Type) where
  hal: VerifyHal Elem ExtElem
  adapter: VerifyAdapter C
  read_iop: ReadIop


class MonadVerify (Elem ExtElem C: Type) (M: Type -> Type)
  extends
    CircuitInfo C,
    TapsProvider C,
    MonadStateOf (VerifyContext Elem ExtElem C) M,
    MonadExceptOf VerificationError M
  where

instance
  [MonadStateOf (VerifyContext Elem ExtElem C) M]
  [MonadExceptOf VerificationError M]
  [CircuitInfo C]
  [TapsProvider C]
  : MonadVerify Elem ExtElem C M where


instance [Monad M] [MonadStateOf (VerifyContext Elem ExtElem C) M] : MonadStateOf ReadIop M where
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

instance [Monad M] [MonadStateOf (VerifyContext Elem ExtElem C) M] : MonadStateOf (VerifyAdapter C) M where
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


def do_verify (Elem ExtElem C: Type) [Monad M] [MonadVerify Elem ExtElem C M]
  (check_code: CheckCode)
  : M Unit
  := do MonadVerifyAdapter.execute
        let taps <- MonadVerifyAdapter.getTaps
        let po2 <- MonadVerifyAdapter.getPo2
        /- assert!(po2 as usize <= MAX_CYCLES_PO2); -/
        let size := 1 <<< po2
        /- let domain = INV_RATE * size; -/
        return ()


def verify [CircuitInfo C] [TapsProvider C]
  (hal: VerifyHal Elem ExtElem) (circuit: C) (seal: Subarray UInt32) (check_code: CheckCode)
  : Id (Except VerificationError Unit)
  := do let verify_context: VerifyContext Elem ExtElem C := {
          hal,
          adapter := VerifyAdapter.new circuit,
          read_iop := ReadIop.new seal
        }
        ExceptT.run (StateT.run' (do_verify Elem ExtElem C check_code) verify_context)

end Zkvm.Verify
