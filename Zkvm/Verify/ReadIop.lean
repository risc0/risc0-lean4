/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import R0sy.Ext.Subarray
import R0sy.Sha2
import Zkvm.Circuit

namespace Zkvm.Verify.ReadIop

open R0sy.Algebra
open R0sy.Ext.Subarray
open R0sy.Sha2

structure ReadIop where
  proof: Subarray UInt32
  rng: Rng256

instance [Monad M] [MonadStateOf ReadIop M] : MonadStateOf Rng256 M where
  get
    := do let read_iop <- get
          return read_iop.rng
  set rng
    := do let read_iop <- get
          set { read_iop with rng }
  modifyGet f
    := do let read_iop <- get
          let (x, rng) := f read_iop.rng
          set { read_iop with rng }
          return x

def ReadIop.new (proof: Subarray UInt32): ReadIop := {
  proof,
  rng := Rng256.new
}

def ReadIop.readU32s [Monad M] [MonadStateOf ReadIop M] (n: Nat): M (Subarray UInt32)
  := do let read_iop <- get
        let (result, proof) := Subarray.take read_iop.proof n
        set { read_iop with proof }
        return result

def ReadIop.readFields [Monad M] [MonadStateOf ReadIop M] (F: Type) [Field F] (n: Nat) (out: Array F := Array.mkEmpty n): M (Array F)
  := match n with
      | 0 => return out
      | n + 1
        => do let u32s <- ReadIop.readU32s (Field.words F)
              let f: F := Field.fromUInt32Words u32s
              ReadIop.readFields F n (out.push f)

def ReadIop.commit [Monad M] [MonadStateOf ReadIop M] (digest: Array UInt32): M Unit
  := Rng256.mix digest

def ReadIop.verifyComplete [Monad M] [MonadStateOf ReadIop M]: M Unit
  := do let self <- get
        if 0 < self.proof.size
          then panic s!"proof.size == {self.proof.size}"
          else return ()

end Zkvm.Verify.ReadIop
