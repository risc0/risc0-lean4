/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Lean.ByteArray
import R0sy.Lean.Nat
import R0sy.Lean.UInt32
import R0sy.Hash
import R0sy.Serial

namespace R0sy.Hash.Sha2

open R0sy.Lean.ByteArray
open R0sy.Lean.Nat
open R0sy.Lean.UInt32
open R0sy.Serial

namespace Sha256

structure Digest where
  h0: UInt32
  h1: UInt32
  h2: UInt32
  h3: UInt32
  h4: UInt32
  h5: UInt32
  h6: UInt32
  h7: UInt32
  deriving BEq

def Digest.new (h0 h1 h2 h3 h4 h5 h6 h7: UInt32): Digest
  := { h0, h1, h2, h3, h4, h5, h6, h7 }

instance : Inhabited Digest where
  default := Digest.new 0 0 0 0 0 0 0 0

def Digest.add (d1 d2: Digest): Digest
  := {
    h0 := d1.h0 + d2.h0
    h1 := d1.h1 + d2.h1
    h2 := d1.h2 + d2.h2
    h3 := d1.h3 + d2.h3
    h4 := d1.h4 + d2.h4
    h5 := d1.h5 + d2.h5
    h6 := d1.h6 + d2.h6
    h7 := d1.h7 + d2.h7
  }

def Digest.xor (d1 d2: Digest): Digest
  := {
    h0 := d1.h0 ^^^ d2.h0
    h1 := d1.h1 ^^^ d2.h1
    h2 := d1.h2 ^^^ d2.h2
    h3 := d1.h3 ^^^ d2.h3
    h4 := d1.h4 ^^^ d2.h4
    h5 := d1.h5 ^^^ d2.h5
    h6 := d1.h6 ^^^ d2.h6
    h7 := d1.h7 ^^^ d2.h7
  }

def Digest.ofArray (d: Array UInt32): Digest
  := Digest.new d[0]! d[1]! d[2]! d[3]! d[4]! d[5]! d[6]! d[7]!

def Digest.ofSubarray (d: Subarray UInt32): Digest
  := Digest.new d[0]! d[1]! d[2]! d[3]! d[4]! d[5]! d[6]! d[7]!

def Digest.toArray (d: Digest): Array UInt32
  := #[ d.h0, d.h1, d.h2, d.h3, d.h4, d.h5, d.h6, d.h7 ]

def Digest.toSubarray (d: Digest): Subarray UInt32
  := (Digest.toArray d).toSubarray

def init_hash: Digest
  := {
    h0 := 0x6a09e667,
    h1 := 0xbb67ae85,
    h2 := 0x3c6ef372,
    h3 := 0xa54ff53a,
    h4 := 0x510e527f,
    h5 := 0x9b05688c,
    h6 := 0x1f83d9ab,
    h7 := 0x5be0cd19
  }

def round_constants: Array UInt32 := #[
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
  0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
  0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
]

def Nat.to_be64 (x: Nat): ByteArray := {
  data := #[
    UInt8.ofNat (x >>> (8*7)),
    UInt8.ofNat (x >>> (8*6)),
    UInt8.ofNat (x >>> (8*5)),
    UInt8.ofNat (x >>> (8*4)),
    UInt8.ofNat (x >>> (8*3)),
    UInt8.ofNat (x >>> (8*2)),
    UInt8.ofNat (x >>> (8*1)),
    UInt8.ofNat x
  ]
}

def prepare (msg: ByteArray): Array UInt32 :=
  let padding :=
    let padding_required :=
      let size := msg.size + 1 + 8
      let rem := size % 64
      if rem == 0 then 0 else 64 - rem
    { data := #[0x80] ++ Array.mkArray padding_required 0x00 }
  let length := Nat.to_be64 (msg.size * 8)
  ByteArray.to_be32 (msg ++ padding ++ length)

def to_chunks (msg: Array UInt32): List (Array UInt32)
  := Id.run do
        let mut out := []
        for chunk in [0:msg.size / 16] do
          let start := chunk * 16
          let stop := start + 16
          out := Array.extract msg start stop :: out
        pure out

def schedule (message: Array UInt32): Array UInt32
  := Id.run do
        let mut w: Array UInt32 := Array.mkEmpty 64
        for i in [0:64] do
          if i < 16
            then w := w.push message[i]!
            else do let w15 := w[i-15]!
                    let w2 := w[i-2]!
                    let s0 := (UInt32.ror w15  7) ^^^ (UInt32.ror w15 18) ^^^ (w15 >>> 3)
                    let s1 := (UInt32.ror w2  17) ^^^ (UInt32.ror w2  19) ^^^ (w2 >>> 10)
                    let w' := w[i-16]! + s0 + w[i-7]! + s1
                    w := w.push w'
        pure w

def compress_loop (chunk: Array UInt32) (state: Digest): Digest
  := Id.run do
        let mut a := state.h0
        let mut b := state.h1
        let mut c := state.h2
        let mut d := state.h3
        let mut e := state.h4
        let mut f := state.h5
        let mut g := state.h6
        let mut h := state.h7
        let w := schedule chunk
        for i in [0:64] do
          let S1 := (UInt32.ror e 6) ^^^ (UInt32.ror e 11) ^^^ (UInt32.ror e 25)
          let ch := (e &&& f) ^^^ ((~~~ e) &&& g)
          let temp1 := h + S1 + ch + round_constants[i]! + w[i]!
          let S0 := (UInt32.ror a 2) ^^^ (UInt32.ror a 13) ^^^ (UInt32.ror a 22)
          let maj := (a &&& b) ^^^ (a &&& c) ^^^ (b &&& c)
          let temp2 := S0 + maj
          h := g
          g := f
          f := e
          e := (d + temp1)
          d := c
          c := b
          b := a
          a := (temp1 + temp2)
        pure (Digest.new a b c d e f g h)

def compress (chunk: Array UInt32) (h: Digest): Digest :=
  if chunk.size != 16 then panic s!"Invalid chunk size: {chunk.size}"
  else
    let j := compress_loop chunk h
    Digest.add h j

def hash (msg: ByteArray): Digest :=
  let padded := prepare msg
  let chunks := to_chunks padded
  List.foldr compress init_hash chunks

def hash_words (x: Subarray UInt32): Digest := hash (ByteArray.of_le32 x)

def hash_pair (x y: Digest): Digest :=
  let chunk := Array.map UInt32.swap_endian (x.toArray ++ y.toArray)
  compress chunk init_hash

def hash_pod [SerialUInt32 X] (pod: Array X): Digest :=
  let chunks :=
    let msg := Array.foldl (fun x y => x ++ Array.map UInt32.swap_endian (SerialUInt32.toUInt32Words y)) #[] pod
    let padding_required :=
      let rem := msg.size % 16
      if rem == 0 then 0 else 16 - rem      
    to_chunks (msg ++ Array.mkArray padding_required 0x00)
  List.foldr compress init_hash chunks


structure Rng where
  pool0: Digest
  pool1: Digest
  pool_used: USize

def Rng.new: Rng := {
  pool0 := Sha256.hash "Hello".toUTF8,
  pool1 := Sha256.hash "World".toUTF8,
  pool_used := 0
}

def Rng.step [Monad M] [MonadStateOf Rng M]: M Unit
  := do let self <- get
        let pool0 := Sha256.hash_pair self.pool0 self.pool1
        let pool1 := Sha256.hash_pair pool0 self.pool1
        set { self with
          pool0,
          pool1,
          pool_used := 0
        }

def Rng.check_pool [Monad M] [MonadStateOf Rng M]: M Unit
  := do let self <- get
        if self.pool_used >= 8
        then Rng.step
        else return ()

def Rng.mix [Monad M] [MonadStateOf Rng M] (val: Digest): M Unit
  := do let self <- get
        let p := Digest.xor self.pool0 val
        set { self with pool0 := p }
        Rng.step

def Rng.nextUInt32 [Monad M] [MonadStateOf Rng M]: M UInt32
  := do Rng.check_pool
        let self <- get
        let out := self.pool0.toArray[self.pool_used]!
        set { self with pool_used := self.pool_used + 1 }
        return out

def Rng.nextUInt64 [Monad M] [MonadStateOf Rng M]: M UInt64
  := do let hi32 <- Rng.nextUInt32
        let lo32 <- Rng.nextUInt32
        let hi := UInt64.ofNat (UInt32.toNat hi32)
        let lo := UInt64.ofNat (UInt32.toNat lo32)
        return (hi <<< 32) ||| lo


namespace Examples

def sha_ex_1_in: ByteArray := "abc".toUTF8
def sha_ex_1_out: Digest := Digest.ofArray #[0xba7816bf, 0x8f01cfea, 0x414140de, 0x5dae2223, 0xb00361a3, 0x96177a9c, 0xb410ff61, 0xf20015ad]
#eval hash sha_ex_1_in == sha_ex_1_out

def sha_ex_2_in: ByteArray := "".toUTF8
def sha_ex_2_out: Digest := Digest.ofArray #[0xe3b0c442, 0x98fc1c14, 0x9afbf4c8, 0x996fb924, 0x27ae41e4, 0x649b934c, 0xa495991b, 0x7852b855]
#eval hash sha_ex_2_in == sha_ex_2_out

def sha_ex_3_in: ByteArray := "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu".toUTF8
def sha_ex_3_out: Digest := Digest.ofArray #[0xcf5b16a7, 0x78af8380, 0x036ce59e, 0x7b049237, 0x0b249b11, 0xe8f07a51, 0xafac4503, 0x7afee9d1]
#eval hash sha_ex_3_in == sha_ex_3_out

def sha_ex_4_in_1: Digest := Digest.ofArray #[0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19]
def sha_ex_4_in_2: Digest := Digest.ofArray #[0xed375cad, 0xc653bb90, 0x78cee904, 0xacee6f7f, 0xf2bf7476, 0xc92dc929, 0x11bae27c, 0x41ebc015]
def sha_ex_4_out: Digest := Digest.ofArray #[0x3aa2c47c, 0x47cd9e5c, 0x5259fd1c, 0x3428c30b, 0x9608201f, 0x5e163061, 0xdeea8d2d, 0x7c65f2c3]
#eval hash_pair sha_ex_4_in_1 sha_ex_4_in_2 == sha_ex_4_out

def sha_ex_5_in: Array UInt32 := #[1]
def sha_ex_5_out: Digest := Digest.ofArray #[0xe3050856, 0xaac38966, 0x1ae49065, 0x6ad0ea57, 0xdf6aff0f, 0xf6eef306, 0xf8cc2eed, 0x4f240249]
#eval hash_pod sha_ex_5_in == sha_ex_5_out

def sha_ex_6_in: Array UInt32 := #[1, 2]
def sha_ex_6_out: Digest := Digest.ofArray #[0x4138ebae, 0x12299733, 0xcc677d11, 0x50c2a013, 0x9454662f, 0xc76ec95d, 0xa75d2bf9, 0xefddc57a]
#eval hash_pod sha_ex_6_in == sha_ex_6_out

def sha_ex_7_in: Array UInt32 := #[0xffffffff]
def sha_ex_7_out: Digest := Digest.ofArray #[0xa3dba037, 0xd5617520, 0x9dfd4191, 0xf727e91c, 0x5feb67e6, 0x5a6ab5ed, 0x4daf0893, 0xc89598c8]
#eval hash_pod sha_ex_7_in == sha_ex_7_out

def sha_ex_8_in: Array UInt32 := #[0xff000001, 0xcc000002]
def sha_ex_8_out: Digest := Digest.ofArray #[0x063e9f8f, 0x3caaf995, 0xb23627ea, 0xaf57b218, 0x36b2986c, 0x99aa9767, 0xbdd1f5b6, 0x5b391101]
#eval hash_words sha_ex_8_in.toSubarray == sha_ex_8_out

def rng256_ex_1_in: StateM Rng (UInt32 × UInt32)
  := do let _ <- Rng.nextUInt32
        let _ <- Rng.nextUInt32
        let _ <- Rng.nextUInt32
        let _ <- Rng.nextUInt32
        let _ <- Rng.nextUInt32
        let _ <- Rng.nextUInt32
        let _ <- Rng.nextUInt32
        let _ <- Rng.nextUInt32
        let _ <- Rng.nextUInt32
        let _ <- Rng.nextUInt32
        let t1 <- Rng.nextUInt32
        Rng.mix (hash "foo".toUTF8)
        let t2 <- Rng.nextUInt32
        return (t1, t2)
def rng256_ex_1_out_1: UInt32 := 1826198275
def rng256_ex_1_out_2: UInt32 := 1753965479
#eval (rng256_ex_1_in Rng.new).1 == (1826198275, 1753965479)

end Examples

end Sha256

instance : SerialUInt32 Sha256.Digest where
  words := 8
  toUInt32Words := Sha256.Digest.toArray
  fromUInt32Words := Sha256.Digest.ofSubarray

instance : Hash Sha256.Digest where
  hash := Sha256.hash
  hash_words := Sha256.hash_words
  hash_pair := Sha256.hash_pair
  hash_pod := Sha256.hash_pod

instance [Monad M] [MonadStateOf Sha256.Rng M]: MonadRng M where
  nextUInt32 := Sha256.Rng.nextUInt32
  nextUInt64 := Sha256.Rng.nextUInt64

instance [Monad M] [MonadStateOf Sha256.Rng M]: MonadMixRng Sha256.Digest M where
  mix := Sha256.Rng.mix


end R0sy.Hash.Sha2
