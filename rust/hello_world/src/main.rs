// Copyright 2022 Risc0, Inc.

use std::path::PathBuf;

use clap::{Parser, Subcommand};
use risc0_zkvm::Prover;

mod disk;
mod lean;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    Disk {
        #[arg(long, value_name = "FILE")]
        out_base: PathBuf,
    },
    Lean {},
}

pub fn main() {
    let cli = Cli::parse();

    let path: &str = hello_world_guard::HELLO_WORLD_PATH;
    let id: &[u8] = hello_world_guard::HELLO_WORLD_ID;

    let image = std::fs::read(path).expect("Could not load image");

    let receipt = {
        let mut prover = Prover::new(&image, id).expect("Could not create prover");

        prover.run().expect("Could not get receipt")
    };

    receipt.verify(id).expect("Could not verify receipt");

    match cli.command {
        Command::Disk { out_base } => {
            disk::save_to_disk(out_base, image, id, receipt).expect("Could not write to disk")
        }
        Command::Lean {} => lean::print_lean_file(id, &receipt),
    }
}
