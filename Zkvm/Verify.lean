/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm.Circuit
import Zkvm.Constants
import Zkvm.MethodId
import Zkvm.Taps
import Zkvm.Verify.Classes
import Zkvm.Verify.Merkle
import Zkvm.Verify.Monad

namespace Zkvm.Verify

open R0sy.Algebra
open R0sy.Hash.Sha2
open Circuit
open Classes
open Merkle
open MethodId
open Taps


structure MerkleVerifiers (ExtElem: Type) where
  code: MerkleTreeVerifier
  data: MerkleTreeVerifier
  accum: MerkleTreeVerifier
  poly_mix: ExtElem
  check: MerkleTreeVerifier

def MerkleVerifiers.check_code_root (Elem ExtElem: Type) [Monad M] [Monad.MonadVerify Elem ExtElem M] (po2: Nat) (code_root: Sha256.Digest): M Unit
  := do let method_id <- MonadMethodId.getMethodId
        let which := po2 - Constants.MIN_CYCLES_PO2
        if which >= method_id.table.size then throw (VerificationError.MethodCycleError po2)
        if method_id.table[which]! != code_root then throw VerificationError.MethodVerificationError
        pure ()

def MerkleVerifiers.new (Elem ExtElem: Type) [Monad M] [Monad.MonadVerify Elem ExtElem M] [PrimeField Elem] [Field ExtElem] (po2: Nat) (taps: TapSet) (domain: Nat): M (MerkleVerifiers ExtElem)
  := do let code <- MerkleTreeVerifier.new domain (TapSet.groupSize taps RegisterGroup.Code) Constants.QUERIES
        MerkleVerifiers.check_code_root Elem ExtElem po2 (MerkleTreeVerifier.root code)
        let data <- MerkleTreeVerifier.new domain (TapSet.groupSize taps RegisterGroup.Data) Constants.QUERIES
        MonadVerifyAdapter.accumulate
        let accum <- MerkleTreeVerifier.new domain (TapSet.groupSize taps RegisterGroup.Accum) Constants.QUERIES
        let poly_mix <- @Field.random ExtElem _ M _ _
        let check <- MerkleTreeVerifier.new domain Constants.CHECK_SIZE Constants.QUERIES
        pure {
          code,
          data,
          accum,
          poly_mix
          check
        }

def verify (Elem ExtElem: Type) [Monad M] [Monad.MonadVerify Elem ExtElem M] [PrimeField Elem] [RootsOfUnity Elem] [Field ExtElem] (journal: Array UInt32)
  : M Unit
  := do -- Initialize the adapter
        MonadVerifyAdapter.execute
        MonadVerifyAdapter.verifyOutput journal
        -- Get the size
        let po2 <- MonadVerifyAdapter.getPo2
        if po2.toNat > Constants.MAX_CYCLES_PO2 then panic s!"po2:{po2} > Constants.MAX_CYCLES_PO2:{Constants.MAX_CYCLES_PO2}"
        let size := (1 <<< po2).toNat
        let domain := Constants.INV_RATE * size
        -- Get taps and compute sizes
        let circuit <- @MonadCircuit.getCircuit Elem ExtElem _ _
        -- Get the code, data, and accum roots
        let merkle <- MerkleVerifiers.new Elem ExtElem po2.toNat circuit.taps domain
        -- Begin the check process
        let z <- @Field.random ExtElem _ M _ _
        let back_one := (@RootsOfUnity.ROU_REV Elem _)[po2.toNat]!
        -- Read the U coeffs + commit their hash
        let num_taps := TapSet.tapSize circuit.taps
        let coeff_u <- MonadReadIop.readFields ExtElem (num_taps + Constants.CHECK_SIZE)
        let hash_u := Sha256.hash_pod coeff_u
        MonadReadIop.commit hash_u
        -- Now convert to evaluated values
        -- TODO: this requires VerifyHal::poly_eval, which (in this case) is just evaluation of coeff_u
        -- Sorry! We have not implemented the rest of the logic yet ;)
        throw (VerificationError.Sorry "Zkvm.Verify.verify")

def run_verify [PrimeField Elem] [RootsOfUnity Elem] [Field ExtElem] (circuit: Circuit Elem ExtElem) (method_id: MethodId) (journal seal: Array UInt32)
  := Monad.VerifyContext.run circuit method_id seal (verify Elem ExtElem journal)

end Zkvm.Verify
