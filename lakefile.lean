import Lake
open Lake DSL

package «zkvm-verify» {
  -- add package configuration options here
}

lean_lib ZkvmVerify {
  -- add library configuration options here
}

@[default_target]
lean_exe «zkvm-verify» {
  root := `Main
}
