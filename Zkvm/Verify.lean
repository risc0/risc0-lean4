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

def MerkleVerifiers.check_code_root (Elem ExtElem: Type) [Algebraic Elem ExtElem] [Monad.MonadVerify Elem ExtElem M] (code: MerkleTreeVerifier): M Unit
  := do let method_id <- MonadMethodId.getMethodId
        let po2 <- MonadVerifyAdapter.get_po2
        let which := po2 - Constants.MIN_CYCLES_PO2
        if which >= method_id.table.size then throw (VerificationError.MethodCycleError po2)
        if method_id.table[which]! != MerkleTreeVerifier.root code then throw VerificationError.MethodVerificationError
        pure ()

def MerkleVerifiers.new (Elem ExtElem: Type) [Algebraic Elem ExtElem] [Monad.MonadVerify Elem ExtElem M] (taps: TapSet): M (MerkleVerifiers ExtElem)
  := do let domain <- MonadVerifyAdapter.get_domain
        let code <- MerkleTreeVerifier.new domain (TapSet.groupSize taps RegisterGroup.Code) Constants.QUERIES
        MerkleVerifiers.check_code_root Elem ExtElem code
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

def verify.enforce_max_cycles (Elem ExtElem: Type) [Algebraic Elem ExtElem] [Monad.MonadVerify Elem ExtElem M]: M Unit
  := do let po2 <- MonadVerifyAdapter.get_po2
        if po2 > Constants.MAX_CYCLES_PO2 then panic s!"po2:{po2} > Constants.MAX_CYCLES_PO2:{Constants.MAX_CYCLES_PO2}"
        pure ()

def verify (Elem ExtElem: Type) [Algebraic Elem ExtElem] [Monad.MonadVerify Elem ExtElem M] (journal: Array UInt32): M Unit
  := do -- Initialize the adapter
        MonadVerifyAdapter.execute
        MonadVerifyAdapter.verifyOutput journal
        -- Enforce constraints on cycle count
        verify.enforce_max_cycles Elem ExtElem
        -- Get taps and compute sizes
        let circuit <- @MonadCircuit.getCircuit Elem ExtElem _ _
        -- Get the code, data, and accum roots
        let merkle <- MerkleVerifiers.new Elem ExtElem circuit.taps
        -- Begin the check process
        let z <- @Field.random ExtElem _ M _ _
        let po2 <- MonadVerifyAdapter.get_po2
        let back_one := (@RootsOfUnity.ROU_REV Elem _)[po2]!
        -- Read the U coeffs + commit their hash
        let num_taps := TapSet.tapSize circuit.taps
        let coeff_u <- MonadReadIop.readFields ExtElem (num_taps + Constants.CHECK_SIZE)
        let hash_u := Sha256.hash_pod coeff_u
        MonadReadIop.commit hash_u
        -- Now convert to evaluated values
        -- TODO: this requires VerifyHal::poly_eval, which (in this case) is just evaluation of coeff_u
        -- Sorry! We have not implemented the rest of the logic yet ;)
        throw (VerificationError.Sorry "Zkvm.Verify.verify")

def run_verify [Algebraic Elem ExtElem] (circuit: Circuit Elem ExtElem) (method_id: MethodId) (journal seal: Array UInt32)
  := Monad.VerifyContext.run circuit method_id seal (verify Elem ExtElem journal)

end Zkvm.Verify
