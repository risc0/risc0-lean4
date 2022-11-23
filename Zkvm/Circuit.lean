/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace Zkvm.Circuit

structure TapSet where
  -- TODO

class CircuitInfo (C: Type) where
  -- TODO

class TapsProvider (C: Type) where
  taps: C -> TapSet

end Zkvm.Circuit
