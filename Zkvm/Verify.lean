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


def check_code [Monad M] [MonadExceptOf VerificationError M] (po2: UInt32) (digest: Sha256.Digest): M Unit
  := throw (VerificationError.Sorry "Zkvm.Verify.check_code")

def verify (Elem ExtElem: Type) [Monad M] [Monad.MonadVerify Elem ExtElem M] [PrimeField Elem] (journal: Array UInt32)
  : M Unit
  := do MonadVerifyAdapter.execute
        MonadVerifyAdapter.verifyOutput journal
        throw (VerificationError.Sorry "Zkvm.Verify.verify")

def run_verify [PrimeField Elem] (circuit: Circuit Elem ExtElem) (journal seal: Array UInt32)
  := Monad.VerifyContext.run circuit seal (verify Elem ExtElem journal)

end Zkvm.Verify
