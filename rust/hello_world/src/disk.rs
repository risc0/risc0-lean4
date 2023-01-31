// Copyright 2022 Risc0, Inc.

use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

use risc0_zkvm::Receipt;

pub fn save_to_disk(
    mut out_base: PathBuf,
    image: &'static [u8],
    id: &'static str,
    receipt: Receipt,
) -> std::io::Result<()> {
    {
        out_base.set_extension("bin");
        File::create(&out_base)?.write_all(&image)?;
    }
    {
        out_base.set_extension("id");
        File::create(&out_base)?.write_all(id.as_bytes())?;
    }
    {
        out_base.set_extension("journal");
        File::create(&out_base)?.write_all(receipt.get_journal_bytes())?;
    }
    {
        out_base.set_extension("seal");
        File::create(&out_base)?.write_all(receipt.get_seal_bytes())?;
    }
    Ok(())
}
