import Lake
open Lake DSL

package «zkvm-verify» {
  -- add package configuration options here
}

lean_lib R0sy {
  -- add library configuration options here
}

lean_lib Zkvm {
  -- add library configuration options here
}

lean_lib Cirgen {
  -- add library configuration options here
}

@[default_target]
lean_exe «zkvm-verify» {
  root := `Main
}

meta if get_config? doc = some "on" then -- do not download and build doc-gen4 by default
require «doc-gen4» from git "https://github.com/leanprover/doc-gen4" @ "main"
  