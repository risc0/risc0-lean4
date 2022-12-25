/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm

open R0sy.Algebra.Field
open R0sy.Lean.ByteArray

def read_file (filename : System.FilePath): IO (Array UInt32)
  := do let meta <- filename.metadata
        let byteSize := meta.byteSize
        let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
        let bytes <- handle.read (byteSize.toNat.toUSize)
        pure (ByteArray.to_le32 bytes)

def check_seal (circuit: Zkvm.ArithVM.Circuit.Circuit) (base_name: String): IO Unit
  := do IO.println s!"Checking {base_name} ..."
        let id <- read_file s!"{base_name}.id"
        let journal <- read_file s!"{base_name}.journal"
        let seal <- read_file s!"{base_name}.seal"
        IO.println s!"ID size:      {id.size} words"
        IO.println s!"Journal size: {journal.size} words"
        IO.println s!"Seal size:    {seal.size} words"
        let method_id := Zkvm.MethodId.MethodId.ofWords R0sy.Hash.Sha2.Sha256.Digest id.toSubarray
        let result := Zkvm.Verify.ReadIop.ReadIop.run seal (Zkvm.Verify.verify circuit method_id journal)
        match result with
        | Except.ok _ => IO.println "Seal is OK"
        | Except.error error => IO.println s!"ERROR: {error}"
        IO.println ""

def read_circuit (filename : System.FilePath): IO Zkvm.ArithVM.Circuit.Circuit
  := do IO.println s!"Reading circuit ..."
        let circuit <- Zkvm.ArithVM.Circuit.Circuit.ofFile .BabyBear2 filename
        IO.println s!"output_size:  {circuit.output_size}"
        IO.println s!"mix_size:     {circuit.mix_size}"
        IO.println s!"TapSet size:  {circuit.taps.taps.size}"
        IO.println s!"Step size:    {circuit.polydef.block.size}"
        IO.println ""
        pure circuit

def main : IO Unit
  := do -- Read the circuit
        let circuit <- read_circuit "rust/output/riscv.circuit"
        -- Check a seal
        check_seal circuit "rust/output/hello_world"
        check_seal circuit "rust/output/hw"
