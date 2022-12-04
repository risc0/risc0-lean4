/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Algebra
import R0sy.Hash.Sha2
import Zkvm.Circuit
import Zkvm.Constants
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
open Taps


def check_code [Monad M] [MonadExceptOf VerificationError M] (po2: UInt32) (digest: Sha256.Digest): M Unit
  := throw (VerificationError.Sorry "Zkvm.Verify.check_code")

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
        let code_merkle <- MerkleTreeVerifier.new domain (TapSet.groupSize circuit.taps RegisterGroup.Code) Constants.QUERIES
        check_code po2 (MerkleTreeVerifier.root code_merkle)
        let data_merkle <- MerkleTreeVerifier.new domain (TapSet.groupSize circuit.taps RegisterGroup.Data) Constants.QUERIES
        MonadVerifyAdapter.accumulate
        let accum_merkle <- MerkleTreeVerifier.new domain (TapSet.groupSize circuit.taps RegisterGroup.Accum) Constants.QUERIES
        -- Begin the check process
        let poly_mix <- @Field.random ExtElem _ M _ _
        let check_merkle <- MerkleTreeVerifier.new domain Constants.CHECK_SIZE Constants.QUERIES
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

def run_verify [PrimeField Elem] [RootsOfUnity Elem] [Field ExtElem] (circuit: Circuit Elem ExtElem) (journal seal: Array UInt32)
  := Monad.VerifyContext.run circuit seal (verify Elem ExtElem journal)

end Zkvm.Verify
