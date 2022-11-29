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

## Building the docs

```console
$ lake -Kdoc=on build Zkvm:docs
```

The docs will be viewable at `build/doc/index.html`.


## Running

Make sure you've generated the example receipt. It should be located in `rust/output`:

```console
risc0-lean4$ ls rust/output
hello_world.bin  hello_world.id  hello_world.journal  hello_world.seal
```

If not, generate it now:

```console
risc0-lean4$ cd rust ; cargo run --release -- disk --out-base=output/hello_world ; cd ..
```

You can now run the `zkvm-verify` demo:

```console
risc0-lean4$ ./build/bin/zkvm-verify
```
