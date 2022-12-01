/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy
import Zkvm

open R0sy.Lean.ByteArray

def read_file (filename : System.FilePath): IO (Array UInt32)
  := do let meta <- filename.metadata
        let byteSize := meta.byteSize
        let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
        let bytes <- handle.read (byteSize.toNat.toUSize)
        pure (ByteArray.to_le32 bytes)

def check_seal (base_name: String): IO Unit
  := do IO.println s!"Checking {base_name} ..."
        let id <- read_file s!"{base_name}.id"
        let journal <- read_file s!"{base_name}.journal"
        let seal <- read_file s!"{base_name}.seal"
        IO.println s!"ID size:      {id.size} words"
        IO.println s!"Journal size: {journal.size} words"
        IO.println s!"Seal size:    {seal.size} words"
        let result := Zkvm.Verify.run_verify Zkvm.Circuit.Riscv.riscv journal seal
        match result with
        | Except.ok _ => IO.println "Seal is OK"
        | Except.error error => IO.println s!"ERROR: {error}"
        IO.println ""

def main : IO Unit
  := do -- Check a seal
        check_seal "rust/output/hello_world"
        check_seal "rust/output/hw"
