/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import Zkvm.Algebra.Classes
import Zkvm.Algebra.BabyBear
import Zkvm.Algebra.Ntt
import Zkvm.ArithVM.AST
import Zkvm.ArithVM.Circuit
import Zkvm.ArithVM.Taps
import Zkvm.Constants
import Zkvm.MethodId
import Zkvm.Platform.Elf
import Zkvm.Platform.Mem
import Zkvm.Seal.CheckCommitments
import Zkvm.Seal.Fri
import Zkvm.Seal.Header
import Zkvm.Seal.TraceCommitments
import Zkvm.Verify
import Zkvm.Verify.Error
import Zkvm.Verify.Merkle
import Zkvm.Verify.ReadIop

/-!
# ZKVM

The RISC Zero ZKVM.
-/

namespace Zkvm
open R0sy.Hash.Sha2
open Zkvm.ArithVM.Circuit
open Zkvm.MethodId
open Zkvm.Verify.Error
open Zkvm.Verify.ReadIop

def verify
    (seal: Array UInt32)
    (circuit: Circuit)
    (methodId: MethodId Sha256.Digest)
    (journal: Array UInt32)
    : Except VerificationError Unit
  := ReadIop.run seal (Zkvm.Verify.verify circuit methodId journal)

end Zkvm
