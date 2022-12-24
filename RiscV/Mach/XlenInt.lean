/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import RiscV.Config

namespace RiscV.Mach.XlenInt

open RiscV.Config

def XlenInt (xlen: Xlen): Type
  := match xlen with
      | .Xlen32 => UInt32
      | .Xlen64 => UInt64

namespace XlenInt
  def min_unsigned: (xlen: Xlen) -> XlenInt xlen
    | .Xlen32 => (0x00000000: UInt32)
    | .Xlen64 => (0x0000000000000000: UInt64)

  def max_unsigned: (xlen: Xlen) -> XlenInt xlen
    | .Xlen32 => (0xffffffff: UInt32)
    | .Xlen64 => (0xffffffffffffffff: UInt64)

  def min_signed: (xlen: Xlen) -> XlenInt xlen
    | .Xlen32 => (0x80000000: UInt32)
    | .Xlen64 => (0x8000000000000000: UInt64)

  def max_signed: (xlen: Xlen) -> XlenInt xlen
    | .Xlen32 => (0x7fffffff: UInt32)
    | .Xlen64 => (0x7fffffffffffffff: UInt64)


  def ofNat: (xlen: Xlen) -> Nat -> XlenInt xlen
    | .Xlen32, val => val.toUInt32
    | .Xlen64, val => val.toUInt64

  def toNat: {xlen: Xlen} -> XlenInt xlen -> Nat
    | .Xlen32, (val: UInt32) => val.toNat
    | .Xlen64, (val: UInt64) => val.toNat

  instance : Inhabited (XlenInt xlen) where default := ofNat xlen 0

  def zero {xlen: Xlen}: XlenInt xlen
    := ofNat xlen 0

  def one {xlen: Xlen}: XlenInt xlen
    := ofNat xlen 1


  def isNeg: {xlen: Xlen} -> XlenInt xlen -> Bool
    | .Xlen32, (val: UInt32) => val >>> 31 == 1
    | .Xlen64, (val: UInt64) => val >>> 63 == 1

  def neg: {xlen: Xlen} -> XlenInt xlen -> XlenInt xlen
    | .Xlen32, (val: UInt32) => (~~~ val) + 1
    | .Xlen64, (val: UInt64) => (~~~ val) + 1


  def ofInt (xlen: Xlen) (val: Int): XlenInt xlen
    := if val < 0
        then (ofNat xlen (- val).toNat).neg
        else ofNat xlen val.toNat

  def toInt {xlen: Xlen} (val: XlenInt xlen): Int
    := if val.isNeg
        then Int.negOfNat val.neg.toNat
        else Int.ofNat val.toNat


  def lt_unsigned {xlen: Xlen} (lhs rhs: XlenInt xlen): Bool
    := lhs.toNat < rhs.toNat

  def ge_unsigned {xlen: Xlen} (lhs rhs: XlenInt xlen): Bool
    := lhs.toNat >= rhs.toNat


  def lt_signed {xlen: Xlen} (lhs rhs: XlenInt xlen): Bool
    := lhs.toInt < rhs.toInt

  def ge_signed {xlen: Xlen} (lhs rhs: XlenInt xlen): Bool
    := lhs.toInt >= rhs.toInt


  def add: {xlen: Xlen} -> XlenInt xlen -> XlenInt xlen -> XlenInt xlen
    | .Xlen32, (lhs: UInt32), (rhs: UInt32) => lhs + rhs
    | .Xlen64, (lhs: UInt64), (rhs: UInt64) => lhs + rhs

  def sub: {xlen: Xlen} -> XlenInt xlen -> XlenInt xlen -> XlenInt xlen
    | .Xlen32, (lhs: UInt32), (rhs: UInt32) => lhs - rhs
    | .Xlen64, (lhs: UInt64), (rhs: UInt64) => lhs - rhs


  def and: {xlen: Xlen} -> XlenInt xlen -> XlenInt xlen -> XlenInt xlen
    | .Xlen32, (lhs: UInt32), (rhs: UInt32) => lhs &&& rhs
    | .Xlen64, (lhs: UInt64), (rhs: UInt64) => lhs &&& rhs

  def or: {xlen: Xlen} -> XlenInt xlen -> XlenInt xlen -> XlenInt xlen
    | .Xlen32, (lhs: UInt32), (rhs: UInt32) => lhs ||| rhs
    | .Xlen64, (lhs: UInt64), (rhs: UInt64) => lhs ||| rhs

  def xor: {xlen: Xlen} -> XlenInt xlen -> XlenInt xlen -> XlenInt xlen
    | .Xlen32, (lhs: UInt32), (rhs: UInt32) => lhs ^^^ rhs
    | .Xlen64, (lhs: UInt64), (rhs: UInt64) => lhs ^^^ rhs

  def not: {xlen: Xlen} -> XlenInt xlen -> XlenInt xlen
    | .Xlen32, (val: UInt32) => (~~~ val: UInt32)
    | .Xlen64, (val: UInt64) => (~~~ val: UInt64)

  def shl: {xlen: Xlen} -> XlenInt xlen -> Nat -> XlenInt xlen
    | .Xlen32, (val: UInt32), sh => val <<< sh.toUInt32
    | .Xlen64, (val: UInt64), sh => val <<< sh.toUInt64

  def shr_unsigned: {xlen: Xlen} -> XlenInt xlen -> Nat -> XlenInt xlen
    | .Xlen32, (val: UInt32), sh => val >>> sh.toUInt32
    | .Xlen64, (val: UInt64), sh => val >>> sh.toUInt64

  def shr_signed {xlen: Xlen} (val: XlenInt xlen) (sh: Nat): XlenInt xlen
    := (shr_unsigned val sh).or (if val.isNeg then ofNat xlen <| ((1 <<< sh) - 1) <<< (xlen.bits - sh) else zero)
end XlenInt

end RiscV.Mach.XlenInt
