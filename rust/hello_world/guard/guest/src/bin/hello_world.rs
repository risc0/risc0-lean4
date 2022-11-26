// Copyright 2022 Risc0, Inc.

#![no_std]
#![no_main]

use risc0_zkvm::guest::env;

risc0_zkvm::entry!(main);

pub fn main() {
    env::commit(b"hello world");
}
