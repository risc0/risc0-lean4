// Copyright 2022 Risc0, Inc.

use clap::{Parser, Subcommand};
use std::path::PathBuf;

use risc0_circuit_rv32im::CircuitImpl;
use risc0_zkp::adapter::TapsProvider;

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

    let circuit = CircuitImpl::new();

    let tapset = circuit.get_taps();

    match cli.command {
        Command::Disk { out_base } => {
            disk::save_to_disk(out_base, tapset).expect("Could not write to disk")
        }
    }
}
