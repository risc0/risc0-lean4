// Copyright 2022 Risc0, Inc.

#![no_std]
#![no_main]

risc0_zkvm::entry!(main);

pub fn main() {
    risc0_zkvm::guest::env::commit(b"hello world");
}
