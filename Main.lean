/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm

open R0sy.Lean.ByteArray

def Circuit: Type := Zkvm.Circuit.Circuit R0sy.Algebra.Field.BabyBear.Elem R0sy.Algebra.Field.BabyBear.ExtElem

def read_tapset (filename: System.FilePath): IO Zkvm.Taps.TapSet
  := do let meta <- filename.metadata
        let byteSize := meta.byteSize
        let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
        let bytes <- handle.read (byteSize.toNat.toUSize)
        let result := R0sy.ByteDeserial.ByteReader.run Zkvm.Taps.TapSet.byteRead bytes.data.toSubarray
        match result with
        | Except.ok tapset => pure tapset
        | Except.error error => panic! s!"ERROR: {error}"

def read_file (filename : System.FilePath): IO (Array UInt32)
  := do let meta <- filename.metadata
        let byteSize := meta.byteSize
        let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
        let bytes <- handle.read (byteSize.toNat.toUSize)
        pure (ByteArray.to_le32 bytes)

def check_seal (circuit: Circuit) (base_name: String): IO Unit
  := do IO.println s!"Checking {base_name} ..."
        let id <- read_file s!"{base_name}.id"
        let journal <- read_file s!"{base_name}.journal"
        let seal <- read_file s!"{base_name}.seal"
        IO.println s!"ID size:      {id.size} words"
        IO.println s!"Journal size: {journal.size} words"
        IO.println s!"Seal size:    {seal.size} words"
        let result := Zkvm.Verify.run_verify circuit journal seal
        match result with
        | Except.ok _ => IO.println "Seal is OK"
        | Except.error error => IO.println s!"ERROR: {error}"
        IO.println ""

def read_circuit (filename : System.FilePath): IO Circuit
  := do IO.println s!"Reading circuit ..."
        let taps <- read_tapset s!"{filename}.tapset"
        IO.println s!"TapSet size:  {taps.taps.size}"
        IO.println ""
        pure (Zkvm.Circuit.Riscv.riscv taps)

def main : IO Unit
  := do -- Read the circuit
        let circuit <- read_circuit "rust/output/circuit"
        -- Check a seal
        check_seal circuit "rust/output/hello_world"
        check_seal circuit "rust/output/hw"
