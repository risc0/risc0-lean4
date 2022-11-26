/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Hash.Sha2
import Zkvm.Circuit
import Zkvm.Taps
import Zkvm.Verify.Classes
import Zkvm.Verify.Monad

namespace Zkvm.Verify

open R0sy.Hash.Sha2
open Classes


def check_code [Monad M] [MonadExceptOf VerificationError M] (po2: UInt32) (digest: Sha256.Digest): M Unit := sorry

def verify (C: Type) [Monad M] [Monad.MonadVerify C M]
  : M Unit
  := do MonadVerifyAdapter.execute
        MonadVerifyAdapter.verifyOutput
        sorry

def run_verify [Circuit.CircuitInfo C] [Taps.TapsProvider C] (circuit: C) (seal: Subarray UInt32)
  := Monad.VerifyContext.run circuit seal (verify C)

end Zkvm.Verify
