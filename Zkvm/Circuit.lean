/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import Zkvm.Taps

namespace Zkvm.Circuit

open Taps

structure Circuit where
  outputSize: Nat
  mixSize: Nat
  taps: TapSet

end Zkvm.Circuit
