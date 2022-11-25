/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import R0sy.Hash.Sha2

namespace Zkvm.Verify.Classes

open R0sy.Algebra
open R0sy.Hash.Sha2

class MonadReadIop (M: Type -> Type) where
  readU32s: Nat -> M (Subarray UInt32)
  readFields (F: Type): [Field F] -> Nat -> M (Array F)
  commit: Sha256.Digest -> M Unit
  verifyComplete: M Unit

end Zkvm.Verify.Classes
