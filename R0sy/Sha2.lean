/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace R0sy.Sha2

def UInt32.swap_endian (x: UInt32): UInt32 :=
  let a0 := x &&& 0xff
  let a1 := (x >>> (8*1)) &&& 0xff
  let a2 := (x >>> (8*2)) &&& 0xff
  let a3 := (x >>> (8*3)) &&& 0xff
  let c0 := a0 <<< (8*3)
  let c1 := a1 <<< (8*2)
  let c2 := a2 <<< (8*1)
  let c3 := a3
  c3 ||| c2 ||| c1 ||| c0

def UInt32.ror (x: UInt32) (n: Nat): UInt32 :=
  let l := x >>> UInt32.ofNat n
  let r := x <<< UInt32.ofNat (32 - n)
  l ||| r

def UInt32.of_be32 (b3 b2 b1 b0: UInt8): UInt32 :=
  let c0 := UInt32.ofNat (b0.val.val)
  let c1 := UInt32.ofNat (b1.val.val) <<< (8*1)
  let c2 := UInt32.ofNat (b2.val.val) <<< (8*2)
  let c3 := UInt32.ofNat (b3.val.val) <<< (8*3)
  c3 ||| c2 ||| c1 ||| c0
  

def Nat.to_be64 (x: Nat): ByteArray := {
  data := #[
    UInt8.ofNat ((x >>> (8*7)) &&& 0xff),
    UInt8.ofNat ((x >>> (8*6)) &&& 0xff),
    UInt8.ofNat ((x >>> (8*5)) &&& 0xff),
    UInt8.ofNat ((x >>> (8*4)) &&& 0xff),
    UInt8.ofNat ((x >>> (8*3)) &&& 0xff),
    UInt8.ofNat ((x >>> (8*2)) &&& 0xff),
    UInt8.ofNat ((x >>> (8*1)) &&& 0xff),
    UInt8.ofNat (x &&& 0xff)
  ]
}

partial def ByteArray.to_be32 (x: ByteArray) (i: Nat := 0) (out: Array UInt32 := #[]): Array UInt32 :=
  if i + 4 <= x.size
  then ByteArray.to_be32 x (i + 4) (out.push (UInt32.of_be32 x[i]! x[i+1]! x[i+2]! x[i+3]!))
  else out

namespace Sha256

def init_hash: Array UInt32 := #[
  0x6a09e667,
  0xbb67ae85,
  0x3c6ef372,
  0xa54ff53a,
  0x510e527f,
  0x9b05688c,
  0x1f83d9ab,
  0x5be0cd19
]

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

def prepare (msg: ByteArray): Array UInt32 :=
  let padding :=
    let padding_required :=
      let size := msg.size + 1 + 8
      let rem := size % 64
      if rem == 0 then 0 else 64 - rem
    { data := #[0x80] ++ Array.mkArray padding_required 0x00 }
  let length := Nat.to_be64 (msg.size * 8)
  ByteArray.to_be32 (msg ++ padding ++ length)

partial def to_chunks (msg: Array UInt32) (start: Nat := 0) (out: List (Array UInt32) := []): List (Array UInt32) :=
  let stop := start + 16
  if stop <= msg.size
  then to_chunks msg stop (Array.extract msg start stop :: out)
  else out

partial def schedule (w: Array UInt32): Array UInt32 :=
  if w.size >= 64 then w
  else
    let i := w.size
    let w15 := w[i-15]!
    let w2 := w[i-2]!
    let s0 := (UInt32.ror w15  7) ^^^ (UInt32.ror w15 18) ^^^ (w15 >>> 3)
    let s1 := (UInt32.ror w2  17) ^^^ (UInt32.ror w2  19) ^^^ (w2 >>> 10)
    let w' := w[i-16]! + s0 + w[i-7]! + s1
    schedule (w.push w')

partial def compress_loop (chunk: Array UInt32) (a b c d e f g h: UInt32) (i: Nat := 0): Array UInt32 :=
  if i >= 64 then #[a, b, c, d, e, f, g, h]
  else
    let w := schedule chunk
    let S1 := (UInt32.ror e 6) ^^^ (UInt32.ror e 11) ^^^ (UInt32.ror e 25)
    let ch := (e &&& f) ^^^ ((~~~ e) &&& g)
    let temp1 := h + S1 + ch + round_constants[i]! + w[i]!
    let S0 := (UInt32.ror a 2) ^^^ (UInt32.ror a 13) ^^^ (UInt32.ror a 22)
    let maj := (a &&& b) ^^^ (a &&& c) ^^^ (b &&& c)
    let temp2 := S0 + maj
    compress_loop w (temp1 + temp2) a b c (d + temp1) e f g (i + 1)

def compress (chunk: Array UInt32) (h: Array UInt32): Array UInt32 :=
  if chunk.size != 16 then panic s!"Invalid chunk size: {chunk.size}"
  else if h.size != 8 then panic s!"Invalid state size: {h.size}"
  else
    let j := compress_loop chunk h[0]! h[1]! h[2]! h[3]! h[4]! h[5]! h[6]! h[7]!
    Array.zipWith h j UInt32.add

end Sha256


def sha256 (msg: ByteArray): Array UInt32 :=
  let padded := Sha256.prepare msg
  let chunks := Sha256.to_chunks padded
  List.foldr Sha256.compress Sha256.init_hash chunks

def sha256_pair (x y: Array UInt32): Array UInt32 :=
  let chunk := Array.map UInt32.swap_endian (x ++ y)
  Sha256.compress chunk Sha256.init_hash


namespace Examples256

def sha_ex_1_in: ByteArray := "abc".toUTF8
def sha_ex_1_out: Array UInt32 := #[0xba7816bf, 0x8f01cfea, 0x414140de, 0x5dae2223, 0xb00361a3, 0x96177a9c, 0xb410ff61, 0xf20015ad]
#eval sha256 sha_ex_1_in == sha_ex_1_out

def sha_ex_2_in: ByteArray := "".toUTF8
def sha_ex_2_out: Array UInt32 := #[0xe3b0c442, 0x98fc1c14, 0x9afbf4c8, 0x996fb924, 0x27ae41e4, 0x649b934c, 0xa495991b, 0x7852b855]
#eval sha256 sha_ex_2_in == sha_ex_2_out

def sha_ex_3_in: ByteArray := "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu".toUTF8
def sha_ex_3_out: Array UInt32 := #[0xcf5b16a7, 0x78af8380, 0x036ce59e, 0x7b049237, 0x0b249b11, 0xe8f07a51, 0xafac4503, 0x7afee9d1]
#eval sha256 sha_ex_3_in == sha_ex_3_out


def sha_ex_4_in_1: Array UInt32 := #[0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19]
def sha_ex_4_in_2: Array UInt32 := #[0xed375cad, 0xc653bb90, 0x78cee904, 0xacee6f7f, 0xf2bf7476, 0xc92dc929, 0x11bae27c, 0x41ebc015]
def sha_ex_4_out: Array UInt32 := #[0x3aa2c47c, 0x47cd9e5c, 0x5259fd1c, 0x3428c30b, 0x9608201f, 0x5e163061, 0xdeea8d2d, 0x7c65f2c3]
#eval sha256_pair sha_ex_4_in_1 sha_ex_4_in_2 == sha_ex_4_out

end Examples256


structure Rng256 where
  pool0: Array UInt32
  pool1: Array UInt32
  pool_used: USize

def Rng256.new: Rng256 := {
  pool0 := sha256 "Hello".toUTF8,
  pool1 := sha256 "World".toUTF8,
  pool_used := 0
}

def Rng256.step [Monad M] [MonadStateOf Rng256 M]: M Unit
  := do let self <- get
        let p := sha256_pair self.pool0 self.pool1
        set { self with
          pool0 := p,
          pool1 := sha256_pair p self.pool1,
          pool_used := 0
        }

def Rng256.check_pool [Monad M] [MonadStateOf Rng256 M]: M Unit
  := do let self <- get
        if self.pool_used >= 8
        then Rng256.step
        else return ()

def Rng256.mix [Monad M] [MonadStateOf Rng256 M] (val: Array UInt32): M Unit
  := do let self <- get
        let p := Array.zipWith self.pool0 val UInt32.xor
        set { self with pool0 := p }
        Rng256.step

def Rng256.next_u32 [Monad M] [MonadStateOf Rng256 M]: M UInt32
  := do Rng256.check_pool
        let self <- get
        let out := self.pool0[self.pool_used]!
        set { self with pool_used := self.pool_used + 1 }
        return out

def Rng256.next_u64 [Monad M] [MonadStateOf Rng256 M]: M UInt64
  := do let hi32 <- Rng256.next_u32
        let lo32 <- Rng256.next_u32
        let hi := UInt64.ofNat (UInt32.toNat hi32)
        let lo := UInt64.ofNat (UInt32.toNat lo32)
        return (hi <<< 32) ||| lo


namespace ExamplesRng256

def rng256_ex_1_in: StateM Rng256 (UInt32 × UInt32)
  := do let _ <- Rng256.next_u32
        let _ <- Rng256.next_u32
        let _ <- Rng256.next_u32
        let _ <- Rng256.next_u32
        let _ <- Rng256.next_u32
        let _ <- Rng256.next_u32
        let _ <- Rng256.next_u32
        let _ <- Rng256.next_u32
        let _ <- Rng256.next_u32
        let _ <- Rng256.next_u32
        let t1 <- Rng256.next_u32
        Rng256.mix (sha256 "foo".toUTF8)
        let t2 <- Rng256.next_u32
        return (t1, t2)
def rng256_ex_1_out_1: UInt32 := 1826198275
def rng256_ex_1_out_2: UInt32 := 1753965479
#eval (rng256_ex_1_in Rng256.new).1 == (1826198275, 1753965479)

end ExamplesRng256

end R0sy.Sha2
