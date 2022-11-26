# risc0-lean4

ZKVM verifier written in [Lean 4](https://leanprover.github.io/). **Under-development.**

## Getting Lean4

See the [Quickstart guide](https://leanprover.github.io/lean4/doc/quickstart.html).

## Building and running

First, check to see if `lake` is in your terminal's search path. If not, you either haven't installed Lean4 yet, or you haven't `source`'d the `elan` environment variables.

```console
$ source ~/.elan/env
```

Now you can perform the build and run the tool:

```console
$ lake build
$ ./build/bin/zkvm-verify
```
