/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace R0sy.Serial

class SerialUInt32 (X: Type) where
  words: Nat
  toUInt32Words: X -> Array UInt32
  fromUInt32Words: Subarray UInt32 -> X

instance : SerialUInt32 UInt32 where
  words := 1
  toUInt32Words x := #[x]
  fromUInt32Words x := x[0]!

end R0sy.Serial
