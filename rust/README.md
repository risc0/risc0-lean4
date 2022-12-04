# hello-world

Generates a receipt for a simple "hello world" guest, then serializes to a Lean file.

## Running

### Saving output to disk in binary format

```console
risc0-lean4/rust$ cargo run --release --bin circuit -- disk --out-base=output/riscv
risc0-lean4/rust$ cargo run --release --bin hello-world -- disk --out-base=output/hello_world
risc0-lean4/rust$ cargo run --release --bin hw -- disk --out-base=output/hw
```
