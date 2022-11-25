/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace R0sy.Hash

class Hash (D: Type) where
  hash: ByteArray -> D
  hash_pair: D -> D -> D

class MonadRng (M: Type -> Type) where
  nextUInt32: M UInt32
  nextUInt64: M UInt64

class MonadMixRng (D: Type) (M: Type -> Type) where
  mix (val: D): M Unit

end R0sy.Hash
