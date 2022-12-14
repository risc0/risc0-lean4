/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace Zkvm.Verify.Error

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

end Zkvm.Verify.Error
