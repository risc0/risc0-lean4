/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra.Field.BabyBear
import Zkvm.Taps

namespace Zkvm.Verify.Hal

open R0sy.Algebra.Field
open Taps

def computePolynomial (u: Subarray BabyBear.ExtElem) (poly_mix: BabyBear.ExtElem) (out mix: Subarray BabyBear.Elem): BabyBear.ExtElem := sorry

def foldEval [Monad M] [MonadStateOf (Array BabyBear.ExtElem) M] (x: BabyBear.ExtElem): BabyBear.ExtElem := sorry

def polyEval (coeffs: Subarray BabyBear.ExtElem) (x: BabyBear.ExtElem): BabyBear.ExtElem := sorry

def friEvalTaps
  (taps: TapSet)
  (mix: BabyBear.ExtElem)
  (combo_u: Subarray BabyBear.ExtElem)
  (check_row: Subarray BabyBear.Elem)
  (back_one: BabyBear.Elem)
  (x: BabyBear.Elem)
  (z: BabyBear.ExtElem)
  (rows: Array (Subarray BabyBear.Elem))
  : BabyBear.ExtElem := sorry

end Zkvm.Verify.Hal
