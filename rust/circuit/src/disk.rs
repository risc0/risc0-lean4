// Copyright 2022 Risc0, Inc.

use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

use risc0_zkp::{
    adapter::{PolyExtStep, PolyExtStepDef},
    taps::{RegisterGroup, TapData, TapSet},
};

fn write_u8(file: &mut File, val: u8) -> std::io::Result<()> {
    file.write_all(&[val])
}

fn write_u16(file: &mut File, val: u16) -> std::io::Result<()> {
    file.write_all(&[(val & 0xff) as u8, ((val >> 8) & 0xff) as u8])
}

fn write_u32(file: &mut File, val: u32) -> std::io::Result<()> {
    file.write_all(&[
        (val & 0xff) as u8,
        ((val >> 8) & 0xff) as u8,
        ((val >> 16) & 0xff) as u8,
        ((val >> 24) & 0xff) as u8,
    ])
}

fn write_u16_array(file: &mut File, val: &[u16]) -> std::io::Result<()> {
    // Length
    write_u32(file, val.len() as u32)?;
    // Data
    for d in val {
        write_u16(file, *d)?;
    }

    Ok(())
}

fn write_usize_array(file: &mut File, val: &[usize]) -> std::io::Result<()> {
    // Length
    write_u32(file, val.len() as u32)?;
    // Data
    for d in val {
        write_u32(file, *d as u32)?;
    }

    Ok(())
}

fn write_register_group(file: &mut File, val: RegisterGroup) -> std::io::Result<()> {
    match val {
        RegisterGroup::Accum => write_u16(file, 0)?,
        RegisterGroup::Code => write_u16(file, 1)?,
        RegisterGroup::Data => write_u16(file, 2)?,
    }

    Ok(())
}

fn write_tapdata(file: &mut File, val: &TapData) -> std::io::Result<()> {
    write_u16(file, val.offset)?;
    write_u16(file, val.back)?;
    write_register_group(file, val.group)?;
    write_u8(file, val.combo)?;
    write_u8(file, val.skip)?;

    Ok(())
}

fn write_tapdatas(file: &mut File, val: &[TapData]) -> std::io::Result<()> {
    // Length
    write_u32(file, val.len() as u32)?;
    // Data
    for tap in val {
        write_tapdata(file, tap)?;
    }

    Ok(())
}

fn write_tapset(file: &mut File, val: &TapSet) -> std::io::Result<()> {
    write_tapdatas(file, val.taps)?;
    write_u16_array(file, val.combo_taps)?;
    write_u16_array(file, val.combo_begin)?;
    write_usize_array(file, &val.group_begin)?;
    write_u32(file, val.combos_count as u32)?;
    write_u32(file, val.reg_count as u32)?;
    write_u32(file, val.tot_combo_backs as u32)?;

    Ok(())
}

fn write_step(file: &mut File, val: &PolyExtStep) -> std::io::Result<()> {
    match val {
        PolyExtStep::Const(value, _loc) => {
            write_u32(file, 1)?;
            write_u32(file, *value as u32)?;
        }
        PolyExtStep::Get(tap, _loc) => {
            write_u32(file, 2)?;
            write_u32(file, *tap as u32)?;
        }
        PolyExtStep::GetGlobal(base, offset, _loc) => {
            write_u32(file, 3)?;
            write_u32(file, *base as u32)?;
            write_u32(file, *offset as u32)?;
        }
        PolyExtStep::Add(x1, x2, _loc) => {
            write_u32(file, 4)?;
            write_u32(file, *x1 as u32)?;
            write_u32(file, *x2 as u32)?;
        }
        PolyExtStep::Sub(x1, x2, _loc) => {
            write_u32(file, 5)?;
            write_u32(file, *x1 as u32)?;
            write_u32(file, *x2 as u32)?;
        }
        PolyExtStep::Mul(x1, x2, _loc) => {
            write_u32(file, 6)?;
            write_u32(file, *x1 as u32)?;
            write_u32(file, *x2 as u32)?;
        }
        PolyExtStep::True(_loc) => {
            write_u32(file, 7)?;
        }
        PolyExtStep::AndEqz(x, val, _loc) => {
            write_u32(file, 8)?;
            write_u32(file, *x as u32)?;
            write_u32(file, *val as u32)?;
        }
        PolyExtStep::AndCond(x, cond, inner, _loc) => {
            write_u32(file, 9)?;
            write_u32(file, *x as u32)?;
            write_u32(file, *cond as u32)?;
            write_u32(file, *inner as u32)?;
        }
    }

    Ok(())
}

fn write_stepdef(file: &mut File, val: &PolyExtStepDef) -> std::io::Result<()> {
    // Block length
    write_u32(file, val.block.len() as u32)?;
    // Block
    for op in val.block {
        write_step(file, op)?;
    }
    // Ret
    write_u32(file, val.ret as u32)?;

    Ok(())
}

pub fn save_to_disk(
    mut out_base: PathBuf,
    output_size: usize,
    mix_size: usize,
    tapset: &TapSet,
    stepdef: &PolyExtStepDef,
) -> std::io::Result<()> {
    out_base.set_extension("circuit");
    let mut file = File::create(&out_base)?;

    write_u32(&mut file, output_size as u32)?;
    write_u32(&mut file, mix_size as u32)?;
    write_tapset(&mut file, tapset)?;
    write_stepdef(&mut file, stepdef)?;

    Ok(())
}
