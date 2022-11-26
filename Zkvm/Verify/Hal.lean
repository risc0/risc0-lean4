/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import Zkvm.Verify.Classes

namespace Zkvm.Verify.Hal

open Classes

structure VerifyHal (Elem ExtElem: Type) where
  -- TODO

instance [Monad M] [MonadStateOf (VerifyHal Elem ExtElem) M] : MonadVerifyHal Elem ExtElem M where
  compute_polynomial := sorry
  fold_eval := sorry
  poly_eval := sorry
  fri_eval_taps := sorry

end Zkvm.Verify.Hal
