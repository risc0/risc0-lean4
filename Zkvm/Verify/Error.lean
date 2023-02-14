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

namespace Zkvm.Verify.Error

inductive VerificationError where
  | MethodIDMismatch
  | TooFewCycles (required: Nat)
  | TooManyCycles (po2 max_po2: Nat)
  | MerkleQueryOutOfRange (idx: Nat) (rows: Nat)
  | FRICommitRoundMismatch
  | MerkleBranchMismatch
  | CheckPolyMismatch (result check: String)
  | JournalHashMismatch (idx: Nat) (seal: UInt32) (journal: UInt32)
  | JournalLengthMismatch (seal_len: Nat) (journal_len: Nat)
  | FriGoalMismatch (query_no: Nat) (goal actual: String)
  | ReadIopIncomplete (words_remaining: Nat)
  deriving Repr

instance : ToString VerificationError where
  toString error
    := match error with
        | .MethodIDMismatch => s!"MethodIDMismatch"
        | .TooFewCycles required => s!"TooFewCycles required:{required}"
        | .TooManyCycles po2 max_po2 => s!"TooManycycles po2:{po2} max_po2:{max_po2}"
        | .MerkleQueryOutOfRange idx rows => s!"MerkleQueryOutOfRange idx:{idx} rows:{rows}"
        | .FRICommitRoundMismatch => s!"FRICommitRoundMismatch"
        | .MerkleBranchMismatch => s!"MerkleBranchMismatch"
        | .CheckPolyMismatch result check => s!"CheckPolyMismatch result:{result} check:{check}"
        | .JournalHashMismatch idx seal journal => s!"JournalHashMismatch idx:{idx} seal:{seal} journal:{journal}"
        | .JournalLengthMismatch seal_len journal_len => s!"JournalLengthMismatch seal_len:{seal_len} journal_len:{journal_len}"
        | .FriGoalMismatch query_no goal actual => s!"FriGoalMismatch query_no:{query_no} goal:{goal} actual:{actual}"
        | .ReadIopIncomplete words_remaining => s!"ReadIopIncomplete words_remaining:{words_remaining}"

end Zkvm.Verify.Error
