/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import R0sy.Lean.Subarray
import R0sy.Hash.Sha2
import Zkvm.Circuit
import R0sy
import Zkvm.ArithVM.Circuit
import Zkvm.Verify.Classes

namespace Zkvm.Verify.ReadIop

open R0sy.Algebra
open R0sy.Lean.Subarray
open R0sy.Hash.Sha2

open Classes

structure ReadIop where
  proof: Subarray UInt32
  rng: Sha256.Rng

instance [Monad M] [MonadStateOf ReadIop M] : MonadStateOf Sha256.Rng M where
  get
    := do let self <- get
          return self.rng
  set rng
    := do let self <- get
          set { self with rng }
  modifyGet f
    := do let self <- get
          let (result, rng) := f self.rng
          set { self with rng }
          return result

def ReadIop.new (proof: Subarray UInt32): ReadIop := {
  proof,
  rng := Sha256.Rng.new
}

def ReadIop.readU32s [Monad M] [MonadStateOf ReadIop M] (n: Nat): M (Subarray UInt32)
  := do let read_iop <- get
        let (result, proof) := Subarray.take read_iop.proof n
        set { read_iop with proof }
        return result

def ReadIop.readPodSlice [Monad M] [MonadStateOf ReadIop M] (F: Type) [SerialUInt32 F] (n: Nat) (out: Array F := Array.mkEmpty n): M (Array F)
  := match n with
      | 0 => return out
      | n + 1
        => do let u32s <- ReadIop.readU32s (SerialUInt32.words F)
              let f: F := SerialUInt32.fromUInt32Words u32s
              ReadIop.readPodSlice F n (out.push f)

def ReadIop.commit [Monad M] [MonadStateOf ReadIop M] (digest: Sha256.Digest): M Unit
  := Sha256.Rng.mix digest

def ReadIop.verifyComplete [Monad M] [MonadStateOf ReadIop M]: M Unit
  := do let self <- get
        if 0 < self.proof.size
          then panic s!"proof.size == {self.proof.size}"
          else return ()

instance [Monad M] [MonadStateOf ReadIop M] : MonadReadIop M where
  readU32s := ReadIop.readU32s
  readPodSlice X := ReadIop.readPodSlice X
  readFields F := ReadIop.readPodSlice F
  commit := ReadIop.commit
  verifyComplete := ReadIop.verifyComplete


end Zkvm.Verify.ReadIop
