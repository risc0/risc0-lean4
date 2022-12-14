/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.ArithVM.Circuit
import Zkvm.Verify.Error

namespace Zkvm.Verify.ReadIop

open R0sy.Algebra
open R0sy.Lean.Subarray
open R0sy.Hash
open R0sy.Hash.Sha2
open R0sy.Serial
open Zkvm.ArithVM.Circuit
open Zkvm.Verify.Error


class MonadReadIop (M: Type -> Type)
  extends
    MonadRng M
  where
    readU32s: Nat -> M (Subarray UInt32)
    readPodSlice (X: Type): [SerialUInt32 X] -> Nat -> M (Array X)
    readFields (F: Type): [Field F] -> Nat -> M (Array F)
    commit: Sha256.Digest -> M Unit
    verifyComplete: M Unit


structure ReadIop where
  proof: Subarray UInt32
  rng: Sha256.Rng


namespace ReadIop
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


  def new (proof: Subarray UInt32): ReadIop := {
    proof,
    rng := Sha256.Rng.new
  }

  def readU32s [Monad M] [MonadStateOf ReadIop M] (n: Nat): M (Subarray UInt32)
    := do let read_iop <- get
          let (result, proof) := Subarray.take read_iop.proof n
          set { read_iop with proof }
          return result

  def readPodSlice [Monad M] [MonadStateOf ReadIop M] (F: Type) [SerialUInt32 F] (n: Nat): M (Array F)
    := do let mut out: Array F := Array.mkEmpty n
          for _ in [0:n] do
            let u32s <- readU32s (SerialUInt32.words F)
            let f: F := SerialUInt32.fromUInt32Words u32s
            out := out.push f
          pure out

  def commit [Monad M] [MonadStateOf ReadIop M] (digest: Sha256.Digest): M Unit
    := Sha256.Rng.mix digest

  def verifyComplete [Monad M] [MonadExceptOf VerificationError M] [MonadStateOf ReadIop M]: M Unit
    := do let self <- get
          if 0 < self.proof.size then throw (VerificationError.ReadIopIncomplete self.proof.size)


  instance [Monad M] [MonadExceptOf VerificationError M] [MonadStateOf ReadIop M] : MonadReadIop M where
    readU32s := ReadIop.readU32s
    readPodSlice X := ReadIop.readPodSlice X
    readFields F := ReadIop.readPodSlice F
    commit := ReadIop.commit
    verifyComplete := ReadIop.verifyComplete

  def run (seal: Array UInt32) (f: {M: Type -> Type} -> [Monad M] -> [MonadExceptOf VerificationError M] -> [MonadReadIop M] -> M Unit): Except VerificationError Unit
    := Id.run do
        let M := StateT ReadIop (ExceptT VerificationError Id)
        ExceptT.run (StateT.run' (@f M _ _ _) (new seal.toSubarray))
end ReadIop

end Zkvm.Verify.ReadIop
