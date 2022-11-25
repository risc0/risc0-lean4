/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Hash.Sha2
import Zkvm.Circuit
import Zkvm.Verify.Adapter

namespace Zkvm.Verify

open R0sy.Hash.Sha2
open Adapter
open Circuit

inductive VerificationError where
  | ReceiptFormatError
  | MethodCycleError (required: UInt64)
  | MethodVerificationError
  | MerkleQueryOutOfRange (idx: UInt64) (rows: UInt64)
  | InvalidProof

class VerifyHal (H: Type) where

def assert_seal_nonempty (seal: Array UInt32): Except VerificationError Unit
 := if seal.size == 0
    then throw VerificationError.ReceiptFormatError
    else return ()

def verify [VerifyHal H] [CircuitInfo C] [TapsProvider C]
  (hal: H) (circuit: C) (seal: Array UInt32) (check_code: UInt32 -> Sha256.Digest -> Except VerificationError Unit)
  : Except VerificationError Unit
  := do /- Make sure the seal is non-empty -/
        assert_seal_nonempty seal
        /- Setup the adapter and the taps -/
        let adapter := VerifyAdapter.new circuit
        -- let taps := adapter.taps
        /- Make IOP -/
        -- let iop: Unit := sorry
        /- Read any execution state -/
        -- adapter.execute iop
        /- Get the size -/
        -- let po0 := adapter.po2
        /- TODO -/
        return ()

end Zkvm.Verify
