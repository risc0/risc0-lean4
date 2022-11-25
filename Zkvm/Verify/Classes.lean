/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import R0sy.Hash
import R0sy.Hash.Sha2
import Zkvm.Circuit
import Zkvm.Taps

namespace Zkvm.Verify.Classes

open R0sy.Algebra
open R0sy.Hash
open R0sy.Hash.Sha2
open Circuit
open Taps

class MonadReadIop (M: Type -> Type) extends MonadRng M where
  readU32s: Nat -> M (Subarray UInt32)
  readFields (F: Type): [Field F] -> Nat -> M (Array F)
  commit: Sha256.Digest -> M Unit
  verifyComplete: M Unit

class MonadVerifyAdapter (M: Type -> Type) where
  getTaps: M TapSet
  getPo2: M UInt32
  execute: M Unit
  accumulate: M Unit

class VerifyHal (H: Type) where

end Zkvm.Verify.Classes