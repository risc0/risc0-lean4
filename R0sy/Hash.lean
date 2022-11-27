/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Serial

namespace R0sy.Hash

open Serial

class Hash (D: Type)
    extends SerialUInt32 D
  where
    hash: ByteArray -> D
    hash_pair: D -> D -> D
    hash_pod: [SerialUInt32 X] -> Array X -> D

class MonadRng (M: Type -> Type) where
  nextUInt32: M UInt32
  nextUInt64: M UInt64

class MonadMixRng (D: Type) (M: Type -> Type) where
  mix (val: D): M Unit

end R0sy.Hash
