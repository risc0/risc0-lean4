# hello-world

Generates a receipt for a simple "hello world" guest, then serializes to a Lean file.

## Running

### Saving output to disk in binary format

```console
risc0-lean4/rust$ cargo run --release -- disk --out-base=output/hello_world
```

### Printing output as Lean file

```console
risc0-lean4/rust$ cargo run --release -- lean
```
