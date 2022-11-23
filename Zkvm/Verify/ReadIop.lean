/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra.Field.BabyBear
import R0sy.Sha2
import Zkvm.Circuit

namespace Zkvm.Verify.ReadIop

structure ReadIop where
  proof: Array UInt32
  rng: R0sy.Sha2.Rng256

end Zkvm.Verify.ReadIop
