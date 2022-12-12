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

## Running the ZKVM verifier

```console
risc0-lean4$ ./build/bin/zkvm-verify-lean4
```

## Running the RV32IM emulator

```console
risc0-lean4$ ./build/bin/rv32im-lean4
```

## Building the docs

```console
$ lake -Kdoc=on build Zkvm:docs
```

The docs will be viewable at `build/doc/index.html`.
