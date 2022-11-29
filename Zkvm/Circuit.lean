/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import Zkvm.Taps

namespace Zkvm.Circuit

open Taps

structure MixState (ExtElem: Type) where
  tot: ExtElem
  mul: ExtElem

structure Circuit (Elem ExtElem: Type) where
  outputSize: Nat
  mixSize: Nat
  taps: TapSet
  poly_ext: ExtElem -> Array ExtElem -> Array (Array Elem) -> MixState ExtElem

end Zkvm.Circuit
