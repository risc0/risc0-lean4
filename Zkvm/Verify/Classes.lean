/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import R0sy.Hash
import R0sy.Hash.Sha2
import Zkvm.Taps

namespace Zkvm.Verify.Classes

open R0sy.Algebra
open R0sy.Hash
open R0sy.Hash.Sha2
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

class MonadVerifyHal (Elem ExtElem: Type) (M: Type -> Type) where
  compute_polynomial
    (u: Array ExtElem)
    (poly_mix: ExtElem)
    (out mix: Array Elem)
    : M ExtElem
  fold_eval
    (x: ExtElem)
    (io: Array ExtElem)
    : M (ExtElem × Array ExtElem)
  poly_eval
    (coeffs: Array ExtElem)
    (x: ExtElem)
    : M ExtElem
  fri_eval_taps
    (taps: TapSet)
    (mix: ExtElem)
    (combo_u: Array ExtElem)
    (check_row: Array Elem)
    (back_one: Elem)
    (x: Elem)
    (z: ExtElem)
    (rows: Array (Array Elem))
    : M ExtElem

end Zkvm.Verify.Classes
