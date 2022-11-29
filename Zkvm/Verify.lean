/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Hash.Sha2
import Zkvm.Circuit
import Zkvm.Verify.Classes
import Zkvm.Verify.Monad

namespace Zkvm.Verify

open R0sy.Hash.Sha2
open Circuit
open Classes


def check_code [Monad M] [MonadExceptOf VerificationError M] (po2: UInt32) (digest: Sha256.Digest): M Unit := sorry

def verify [Monad M] [Monad.MonadVerify M]
  : M Unit
  := do MonadVerifyAdapter.execute
        MonadVerifyAdapter.verifyOutput
        sorry

def run_verify (circuit: Circuit) (seal: Subarray UInt32)
  := Monad.VerifyContext.run circuit seal verify

end Zkvm.Verify
