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
open Zkvm.Verify.ReadIop

def verify
    (seal: Array UInt32)
    (circuit: Circuit)
    (methodId: MethodId Sha256.Digest)
    (journal: Array UInt32)
    : Bool
  := (ReadIop.run seal (Zkvm.Verify.verify circuit methodId journal)).toBool

end Zkvm
