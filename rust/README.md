# hello-world

Generates a receipt for a simple "hello world" guest, then serializes to a Lean file.

## Running

### Saving the circuit to disk

Note: this requires using a branch of risc0 where `risc0_circuit_rv32im::poly_ext::DEF` is public.

```console
risc0-lean4/rust$ cargo run --release --bin circuit -- disk --out-base=output/riscv
```

### Saving receipts to disk

```console
risc0-lean4/rust$ cargo run --release --bin hello-world -- disk --out-base=output/hello_world
risc0-lean4/rust$ cargo run --release --bin hw -- disk --out-base=output/hw
```
