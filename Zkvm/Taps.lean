/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace Zkvm.Taps

structure TapSet where
  -- TODO

class TapsProvider (C: Type) where
  taps: C -> TapSet

end Zkvm.Taps
