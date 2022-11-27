/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import R0sy.Algebra.Field.BabyBear
import R0sy.Hash
import R0sy.Hash.Sha2
import R0sy.Lean.UInt32
import R0sy.Serial

namespace R0sy.Test.Hash

open R0sy.Algebra
open R0sy.Algebra.Field
open R0sy.Hash
open R0sy.Hash.Sha2
open R0sy.Lean.UInt32
open R0sy.Serial

def ex_0: Array UInt32 := #[ 0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19 ]
def ex_1: Array UInt32 := #[ 0xda5698be, 0x17b9b469, 0x62335799, 0x779fbeca, 0x8ce5d491, 0xc0d26243, 0xbafef9ea, 0x1837a9d8 ]
def ex_7: Array UInt32 := #[ 0x643f71da, 0xb15c4f6a, 0x6e8820de, 0xe5f59cc0, 0x7818b9c4, 0x473b47bb, 0xa9516cc3, 0xbe992f1c ]
def ex_8: Array UInt32 := #[ 0x3dae5357, 0x5097f63d, 0x0a461048, 0x813cc9ab, 0x870f0ddb, 0xcf9e4aea, 0x8dcddecc, 0x0aea736d ]
def ex_9: Array UInt32 := #[ 0x903fe671, 0xa0971f6d, 0xea6e8a11, 0x80dcd1ce, 0x87b56d0b, 0x42ee3861, 0x212e8642, 0x8a983a5b ]

#eval
  let items: Array BabyBear.Elem := Array.map Ring.ofNat #[]
  let d: Sha256.Digest := Hash.hash_pod items
  d.toArray == ex_0

#eval
  let items: Array BabyBear.Elem := Array.map Ring.ofNat #[0]
  let d: Sha256.Digest := Hash.hash_pod items
  d.toArray == ex_1

#eval
  let items: Array BabyBear.Elem := Array.map Ring.ofNat #[0, 1, 2, 3, 4, 5, 6]
  let d: Sha256.Digest := Hash.hash_pod items
  d.toArray == ex_7

#eval
  let items: Array BabyBear.Elem := Array.map Ring.ofNat #[0, 1, 2, 3, 4, 5, 6, 7]
  let d: Sha256.Digest := Hash.hash_pod items
  d.toArray == ex_8

#eval
  let items: Array BabyBear.Elem := Array.map Ring.ofNat #[0, 1, 2, 3, 4, 5, 6, 7, 8]
  let d: Sha256.Digest := Hash.hash_pod items
  d.toArray == ex_9


def ex_ext_0: Array UInt32 := #[ 0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19 ]
def ex_ext_1: Array UInt32 := #[ 0x6343c9ca, 0x9260f2d6, 0xcf190c2d, 0x2bbff0bf, 0x928789e4, 0xd2c1a246, 0x54137a5d, 0x48f254bc ]
def ex_ext_7: Array UInt32 := #[ 0x07d3bfa6, 0x50095307, 0x90a51cca, 0x21b83dd4, 0x92c60ade, 0x96ee1d2c, 0x5b25c4c5, 0xcfe257b0 ]
def ex_ext_8: Array UInt32 := #[ 0x60a53ad4, 0x2dfe03c7, 0xc0d1d467, 0x90a83235, 0x6d09b52c, 0x6812eada, 0x27622476, 0xd6180392 ]
def ex_ext_9: Array UInt32 := #[ 0x5af62d03, 0x03208f45, 0x73656ac7, 0x07d7447f, 0x0303fd76, 0xa134a775, 0xf329104d, 0x03c37985 ]

#eval
  let input: Array (Subarray UInt32) := Array.map Array.toSubarray #[]
  let items: Array BabyBear.ExtElem := Array.map SerialUInt32.fromUInt32Words input
  let d: Sha256.Digest := Hash.hash_pod items
  d.toArray == ex_ext_0

#eval
  let items: Array BabyBear.ExtElem := #[
    BabyBear.ExtElem.new (Ring.ofNat 0) (Ring.ofNat 1) (Ring.ofNat 2) (Ring.ofNat 3)
  ]
  let d: Sha256.Digest := Hash.hash_pod items
  d.toArray == ex_ext_1

#eval
  let items: Array BabyBear.ExtElem := #[
      BabyBear.ExtElem.new (Ring.ofNat 0)  (Ring.ofNat 1)  (Ring.ofNat 2)  (Ring.ofNat 3),
      BabyBear.ExtElem.new (Ring.ofNat 4)  (Ring.ofNat 5)  (Ring.ofNat 6)  (Ring.ofNat 7),
      BabyBear.ExtElem.new (Ring.ofNat 8)  (Ring.ofNat 9)  (Ring.ofNat 10) (Ring.ofNat 11),
      BabyBear.ExtElem.new (Ring.ofNat 12) (Ring.ofNat 13) (Ring.ofNat 14) (Ring.ofNat 15),
      BabyBear.ExtElem.new (Ring.ofNat 16) (Ring.ofNat 17) (Ring.ofNat 18) (Ring.ofNat 19),
      BabyBear.ExtElem.new (Ring.ofNat 20) (Ring.ofNat 21) (Ring.ofNat 22) (Ring.ofNat 23),
      BabyBear.ExtElem.new (Ring.ofNat 24) (Ring.ofNat 25) (Ring.ofNat 26) (Ring.ofNat 27)
    ]
  let d: Sha256.Digest := Hash.hash_pod items
  d.toArray == ex_ext_7

#eval
  let items: Array BabyBear.ExtElem := #[
      BabyBear.ExtElem.new (Ring.ofNat 0)  (Ring.ofNat 1)  (Ring.ofNat 2)  (Ring.ofNat 3),
      BabyBear.ExtElem.new (Ring.ofNat 4)  (Ring.ofNat 5)  (Ring.ofNat 6)  (Ring.ofNat 7),
      BabyBear.ExtElem.new (Ring.ofNat 8)  (Ring.ofNat 9)  (Ring.ofNat 10) (Ring.ofNat 11),
      BabyBear.ExtElem.new (Ring.ofNat 12) (Ring.ofNat 13) (Ring.ofNat 14) (Ring.ofNat 15),
      BabyBear.ExtElem.new (Ring.ofNat 16) (Ring.ofNat 17) (Ring.ofNat 18) (Ring.ofNat 19),
      BabyBear.ExtElem.new (Ring.ofNat 20) (Ring.ofNat 21) (Ring.ofNat 22) (Ring.ofNat 23),
      BabyBear.ExtElem.new (Ring.ofNat 24) (Ring.ofNat 25) (Ring.ofNat 26) (Ring.ofNat 27),
      BabyBear.ExtElem.new (Ring.ofNat 28) (Ring.ofNat 29) (Ring.ofNat 30) (Ring.ofNat 31)
    ]
  let d: Sha256.Digest := Hash.hash_pod items
  d.toArray == ex_ext_8

#eval
  let items: Array BabyBear.ExtElem := #[
      BabyBear.ExtElem.new (Ring.ofNat 0)  (Ring.ofNat 1)  (Ring.ofNat 2)  (Ring.ofNat 3),
      BabyBear.ExtElem.new (Ring.ofNat 4)  (Ring.ofNat 5)  (Ring.ofNat 6)  (Ring.ofNat 7),
      BabyBear.ExtElem.new (Ring.ofNat 8)  (Ring.ofNat 9)  (Ring.ofNat 10) (Ring.ofNat 11),
      BabyBear.ExtElem.new (Ring.ofNat 12) (Ring.ofNat 13) (Ring.ofNat 14) (Ring.ofNat 15),
      BabyBear.ExtElem.new (Ring.ofNat 16) (Ring.ofNat 17) (Ring.ofNat 18) (Ring.ofNat 19),
      BabyBear.ExtElem.new (Ring.ofNat 20) (Ring.ofNat 21) (Ring.ofNat 22) (Ring.ofNat 23),
      BabyBear.ExtElem.new (Ring.ofNat 24) (Ring.ofNat 25) (Ring.ofNat 26) (Ring.ofNat 27),
      BabyBear.ExtElem.new (Ring.ofNat 28) (Ring.ofNat 29) (Ring.ofNat 30) (Ring.ofNat 31),
      BabyBear.ExtElem.new (Ring.ofNat 32) (Ring.ofNat 33) (Ring.ofNat 34) (Ring.ofNat 35)
    ]
  let d: Sha256.Digest := Hash.hash_pod items
  d.toArray == ex_ext_9

end R0sy.Test.Hash
