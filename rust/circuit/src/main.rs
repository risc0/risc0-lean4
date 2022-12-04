// Copyright 2022 Risc0, Inc.

use std::path::PathBuf;

use clap::{Parser, Subcommand};
use risc0_circuit_rv32im::{poly_ext, CircuitImpl};
use risc0_zkp::adapter::{CircuitInfo, TapsProvider};

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

    let output_size = CircuitImpl::OUTPUT_SIZE;
    let mix_size = CircuitImpl::MIX_SIZE;
    let tapset = circuit.get_taps();
    let stepdef = &poly_ext::DEF;

    match cli.command {
        Command::Disk { out_base } => {
            disk::save_to_disk(out_base, output_size, mix_size, tapset, stepdef)
                .expect("Could not write to disk")
        }
    }
}
