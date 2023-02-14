/-
 Copyright 2023 RISC Zero, Inc.

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
-/

import R0sy.Serial
import R0sy.Lean.UInt32


namespace R0sy.Hash

open R0sy.Lean.UInt32
open Serial


class Hash (D: Type)
  extends
    BEq D,
    Inhabited D,
    SerialUInt32 D
  where
    hash: ByteArray -> D
    hash_words: Subarray UInt32 -> D
    hash_pair: D -> D -> D
    hash_array_array: Array (Array UInt32) -> D

def Hash.hash_pod [Hash D] [SerialUInt32 X] (xs : Array X) : D := hash_array_array (Array.map SerialUInt32.toUInt32Words xs)

class MonadRng (M: Type -> Type) where
  nextUInt32: M UInt32
  nextUInt64: M UInt64

class MonadMixRng (D: Type) (M: Type -> Type) where
  mix (val: D): M Unit

end R0sy.Hash
