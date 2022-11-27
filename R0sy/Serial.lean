/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace R0sy.Serial

class SerialUInt32 (X: Type) where
  words: Nat
  toUInt32Words: X -> Array UInt32
  fromUInt32Words: Subarray UInt32 -> X

end R0sy.Serial
