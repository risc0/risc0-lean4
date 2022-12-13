/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.ArithVM.Circuit
import Zkvm.Constants
import Zkvm.MethodId
import Zkvm.ArithVM.Taps

namespace Zkvm.Verify.Classes

open R0sy.Algebra
open R0sy.Hash
open R0sy.Hash.Sha2
open R0sy.Serial
open ArithVM.Circuit
open ArithVM.Taps
open MethodId


inductive VerificationError where
  | Sorry (msg: String)
  | ReceiptFormatError
  | MethodCycleError (required: Nat)
  | MethodVerificationError
  | MerkleQueryOutOfRange (idx: Nat) (rows: Nat)
  | TooManyCycles (po2 max_po2: Nat)
  | InvalidProof
  | InvalidCheck (result check: String)
  | JournalSealRootMismatch (idx: Nat) (seal: UInt32) (journal: UInt32)
  | SealJournalLengthMismatch (seal_len: Nat) (journal_len: Nat)
  | FriGoalMismatch (query_no: Nat) (goal actual: String)
  | ReadIopIncomplete (words_remaining: Nat)
  deriving Repr

instance : ToString VerificationError where
  toString error
    := match error with
        | VerificationError.Sorry msg => s!"Sorry msg:{msg}"
        | VerificationError.ReceiptFormatError => s!"ReceiptFormatError"
        | VerificationError.MethodCycleError required => s!"MethodCycleError required:{required}"
        | VerificationError.MethodVerificationError => s!"MethodVerificationError"
        | VerificationError.MerkleQueryOutOfRange idx rows => s!"MerkleQueryOutOfRange idx:{idx} rows:{rows}"
        | VerificationError.TooManyCycles po2 max_po2 => s!"TooManycycles po2:{po2} max_po2:{max_po2}"
        | VerificationError.InvalidProof => s!"InvalidProof"
        | VerificationError.InvalidCheck result check => s!"InvalidProof result:{result} check:{check}"
        | VerificationError.JournalSealRootMismatch idx seal journal => s!"JournalSealRootMismatch idx:{idx} seal:{seal} journal:{journal}"
        | VerificationError.SealJournalLengthMismatch seal_len journal_len => s!"SealJournalLengthMismatch seal_len:{seal_len} journal_len:{journal_len}"
        | VerificationError.FriGoalMismatch query_no goal actual => s!"FriGoalMismatch query_no:{query_no} goal:{goal} actual:{actual}"
        | VerificationError.ReadIopIncomplete words_remaining => s!"ReadIopIncomplete words_remaining:{words_remaining}"


class MonadReadIop (M: Type -> Type) extends MonadRng M where
  readU32s: Nat -> M (Subarray UInt32)
  readPodSlice (X: Type): [SerialUInt32 X] -> Nat -> M (Array X)
  readFields (F: Type): [Field F] -> Nat -> M (Array F)
  commit: Sha256.Digest -> M Unit
  verifyComplete: M Unit


class MonadVerifyAdapter (M: Type -> Type) (Elem: outParam Type) where
  get_mix: M (Array Elem)
  accumulate: M Unit


class MonadCircuit (M: Type -> Type) (Elem ExtElem: outParam Type) where
  getCircuit: M (Circuit Elem ExtElem)


class MonadMethodId (M: Type -> Type) where
  getMethodId: M MethodId

end Zkvm.Verify.Classes
