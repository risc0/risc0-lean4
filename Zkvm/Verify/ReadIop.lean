/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra.Field.BabyBear
import R0sy.Ext.Subarray
import R0sy.Sha2
import Zkvm.Circuit

namespace Zkvm.Verify.ReadIop

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

def ReadIop.read_u32s [Monad M] [MonadStateOf ReadIop M] (n: Nat): M (Subarray UInt32)
  := do let read_iop <- get
        let (result, proof) := Subarray.take read_iop.proof n
        set { read_iop with proof }
        return result

end Zkvm.Verify.ReadIop
