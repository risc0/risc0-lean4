# risc0-lean4

ZKVM verifier written in [Lean 4](https://leanprover.github.io/). **Under-development.**


## Getting Lean4

See the [Quickstart guide](https://leanprover.github.io/lean4/doc/quickstart.html).


## Building

First, check to see if `lake` is in your terminal's search path. If not, you either haven't installed Lean4 yet, or you haven't `source`'d the `elan` environment variables.

```console
risc0-lean4$ source ~/.elan/env
```

Now you can perform the build:

```console
risc0-lean4$ lake build
```

## Running the tools

### ZKVM verifier

```console
risc0-lean4$ ./build/bin/zkvm-verify-lean4
```

### ZKVM emulator

```console
risc0-lean4$ ./build/bin/zkvm-emu-lean4
```

### Elf dumper

```console
risc0-lean4$ ./build/bin/elf-dump-lean4
```

## Building the docs

```console
$ lake -Kdoc=on update
$ lake -Kdoc=on build Elf:docs R0sy:docs RiscV:docs Zkvm:docs Mathlib:docs
```

The docs will be viewable at `build/doc/index.html`.
