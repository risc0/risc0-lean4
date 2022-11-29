/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import R0sy.Hash.Sha2
import Zkvm.Circuit
import Zkvm.Verify.Classes
import Zkvm.Verify.Monad

namespace Zkvm.Verify

open R0sy.Algebra
open R0sy.Hash.Sha2
open Circuit
open Classes


def check_code [Monad M] [MonadExceptOf VerificationError M] (po2: UInt32) (digest: Sha256.Digest): M Unit := sorry

def verify (Elem ExtElem: Type) [Monad M] [Monad.MonadVerify Elem ExtElem M] [Field Elem]
  : M Unit
  := do MonadVerifyAdapter.execute
        MonadVerifyAdapter.verifyOutput
        sorry

def run_verify [Field Elem] (circuit: Circuit Elem ExtElem) (seal: Subarray UInt32)
  := Monad.VerifyContext.run circuit seal (verify Elem ExtElem)

end Zkvm.Verify
