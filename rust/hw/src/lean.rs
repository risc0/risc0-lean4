// Copyright 2022 Risc0, Inc.

use risc0_zkvm::Receipt;

fn print_bytes(data: &[u8]) {
    print!("    ");
    for i in (0..data.len()).step_by(4) {
        let more_to_come = i + 4 < data.len();
        let sep = if more_to_come { ", " } else { "" };
        print!(
            "0x{:02x?}{:02x?}{:02x?}{:02x?}{}",
            data[i + 3],
            data[i + 2],
            data[i + 1],
            data[i],
            sep
        );
        if 0 < i && more_to_come && (i + 4) % 32 == 0 {
            println!("");
            print!("    ");
        }
    }
    println!("");
}

pub fn print_lean_file(id: &[u8], receipt: &Receipt, print_seal: bool) {
    let journal = receipt.get_journal_bytes();
    let seal = receipt.get_seal_bytes();

    let namespace = "Zkvm.Test.HW";

    println!("/-");
    println!("Copyright (c) 2022 RISC Zero. All rights reserved.");
    println!("-/");

    println!("");
    println!("");

    println!("namespace {}", namespace);

    println!("");

    println!("def ID: Array UInt32 := #[");
    print_bytes(id);
    println!("]");

    println!("");

    println!("def JOURNAL: Array UInt32 := #[");
    print_bytes(journal);
    println!("]");

    println!("");

    if print_seal {
        let seal_parts = {
            let mut seal_parts = 0;
            let chunk_size = 1024;

            println!("namespace SEAL_PARTS");
            println!("");
            for chunk in (0..seal.len()).step_by(chunk_size) {
                let remaining = seal.len() - chunk;
                let size = usize::min(chunk_size, remaining);

                println!("def PART_{}: Array UInt32 := #[", seal_parts);
                print_bytes(&seal[chunk..chunk + size]);
                println!("]");
                println!("");

                seal_parts += 1;
            }
            println!("end SEAL_PARTS");
            println!("");

            seal_parts
        };

        println!("def SEAL: Array (Array UInt32) := #[");
        for i in 0..seal_parts {
            let more_to_come = i + 1 < seal_parts;
            let sep = if more_to_come { "," } else { "" };
            println!("  SEAL_PARTS.PART_{}{}", i, sep)
        }
        println!("]");

        println!("");
    }

    println!("end {}", namespace);
}
