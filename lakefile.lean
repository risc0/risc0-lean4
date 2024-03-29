import Lake
open Lake DSL

package «risc0-lean4» {
  -- add package configuration options here
}

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"

@[default_target]
lean_lib Elf {
  -- add library configuration options here
}

@[default_target]
lean_exe «elf-dump-lean4» {
  root := `Elf.Main
}

@[default_target]
lean_lib R0sy {
  -- add library configuration options here
}

@[default_target]
lean_lib RiscV {
  -- add library configuration options here
}

@[default_target]
lean_lib Zkvm {
  -- add library configuration options here
}

lean_lib Soundness

@[default_target]
lean_exe «zkvm-verify-lean4» {
  root := `Zkvm.MainVerify
}

@[default_target]
lean_exe «zkvm-emu-lean4» {
  root := `Zkvm.MainEmu
}

meta if get_config? doc = some "on" then -- do not download and build doc-gen4 by default
require «doc-gen4» from git "https://github.com/leanprover/doc-gen4" @ "main"
