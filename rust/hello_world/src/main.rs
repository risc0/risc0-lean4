// Copyright 2022 Risc0, Inc.

use std::path::PathBuf;

use clap::{Parser, Subcommand};
use risc0_zkvm::Prover;

mod disk;

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
}

pub fn main() {
    let cli = Cli::parse();

    let image: &[u8] = hello_world_guard::HELLO_WORLD_ELF;
    let id: &str = hello_world_guard::HELLO_WORLD_ID;

    let receipt = {
        let mut prover = Prover::new(image, id).expect("Could not create prover");

        prover.run().expect("Could not get receipt")
    };

    receipt.verify(id).expect("Could not verify receipt");

    match cli.command {
        Command::Disk { out_base } => {
            disk::save_to_disk(out_base, image, id, receipt).expect("Could not write to disk")
        }
    }
}
