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

def main : IO Unit
  := do IO.println s!"BabyBear has characteristic {R0sy.Algebra.Field.BabyBear.P.rep}"
        IO.println s!"Goldilocks has characteristic {R0sy.Algebra.Field.Goldilocks.P.rep}"
        let expected_id := Zkvm.Test.HelloWorld.ID
        let expected_journal := Zkvm.Test.HelloWorld.JOURNAL
        let example_id <- read_file "rust/output/hello_world.id"
        let example_journal <- read_file "rust/output/hello_world.journal"
        let example_seal <- read_file "rust/output/hello_world.seal"
        IO.println s!"ID size:      {example_id.size} words"
        IO.println s!"Journal size: {example_journal.size} words"
        IO.println s!"Seal size:    {example_seal.size} words"
        IO.println ""
        IO.println s!"ID test:      {example_id == expected_id}"
        IO.println s!"Journal test: {example_journal == expected_journal}"
        IO.println ""
        IO.println ""
        let result := Zkvm.Verify.run_verify Zkvm.Circuit.Riscv.riscv example_journal.toSubarray example_seal.toSubarray
        match result with
        | Except.ok _ => IO.println "Seal is OK"
        | Except.error error => IO.println s!"Seal is not OK: {error}"