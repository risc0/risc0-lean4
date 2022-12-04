/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.ByteDeserial

namespace Zkvm.Taps

open R0sy.ByteDeserial

/- Register groups -/

inductive RegisterGroup where
  | Accum
  | Code
  | Data
  deriving Inhabited

def RegisterGroup.toNat (x: RegisterGroup): Nat
  := match x with
      | RegisterGroup.Accum => 0
      | RegisterGroup.Code => 1
      | RegisterGroup.Data => 2

def RegisterGroup.byteRead [Monad M] [MonadByteReader M]: M RegisterGroup
  := do let val <- MonadByteReader.readUInt16le
        match val with
        | 0 => pure RegisterGroup.Accum
        | 1 => pure RegisterGroup.Code
        | 2 => pure RegisterGroup.Data
        | _ => throw ByteReaderError.InvalidData

def REGISTER_GROUPS: Array RegisterGroup := #[
  RegisterGroup.Accum,
  RegisterGroup.Code,
  RegisterGroup.Data
]


/- TapData -/

structure TapData where
  offset: UInt16
  back: UInt16
  group: RegisterGroup
  combo: UInt8
  skip: UInt8
  deriving Inhabited

def TapData.byteRead [Monad M] [MonadByteReader M]: M TapData
  := do let offset <- MonadByteReader.readUInt16le
        let back <- MonadByteReader.readUInt16le
        let group <- RegisterGroup.byteRead
        let combo <- MonadByteReader.readUInt8
        let skip <- MonadByteReader.readUInt8
        pure {
          offset,
          back,
          group,
          combo,
          skip
        }


/- TapIter -/

structure TapIter where
  iter_data: Subarray TapData
  iter_cursor: Nat
  iter_end: Nat


/- RegRef -/

structure RegRef where
  data: Subarray TapData
  cursor: Nat

def RegRef.group (self: RegRef): RegisterGroup := self.data[self.cursor]!.group

def RegRef.offset (self: RegRef): Nat := self.data[self.cursor]!.offset.toNat

def RegRef.combo_id (self: RegRef): Nat := self.data[self.cursor]!.combo.toNat

def RegRef.size (self: RegRef): Nat := self.data[self.cursor]!.skip.toNat

def RegRef.back (self: RegRef) (i: Nat): Nat := self.data[self.cursor + i]!.back.toNat

def RegRef.into_iter (self: RegRef): TapIter := {
  iter_data := self.data
  iter_cursor := self.cursor
  iter_end := self.cursor + RegRef.size self
}


/- RegIter -/

structure RegIter where
  iter_data: Subarray TapData
  iter_cursor: Nat
  iter_end: Nat

partial def RegIter.forIn [Monad M] [Inhabited X] (self: RegIter) (x: X) (f: RegRef -> X -> M (ForInStep X)): M X
  := do let cursor := self.iter_cursor
        if cursor >= self.iter_data.size then return x
        let next := cursor + self.iter_data[cursor]!.skip.toNat
        if next > self.iter_end then return x
        let step <- f { data := self.iter_data, cursor } x
        match step with
        | ForInStep.done x' => pure x'
        | ForInStep.yield x' => RegIter.forIn { self with iter_cursor := next } x' f

instance : ForIn M RegIter RegRef where
  forIn iter b f := @RegIter.forIn _ _ _ { default := b } iter b f


/- Combo -/

structure ComboData where
  taps: Subarray UInt16
  offsets: Subarray UInt16


structure ComboIter where
  iter_data: ComboData
  iter_id: Nat
  iter_end: Nat


structure ComboRef where
  data: ComboData
  id: Nat


/- TapSet -/

structure TapSet where
  taps: Array TapData
  combo_taps: Array UInt16
  combo_begin: Array UInt16
  group_begin: Array UInt32
  combos_count: UInt32
  reg_count: UInt32
  tot_combo_backs: UInt32
  deriving Inhabited

def TapSet.byteRead [Monad M] [MonadByteReader M]: M TapSet
  := do let taps <- MonadByteReader.readArray TapData.byteRead
        let combo_taps <- MonadByteReader.readUInt16le_array
        let combo_begin <- MonadByteReader.readUInt16le_array
        let group_begin <- MonadByteReader.readUInt32le_array
        let combos_count <- MonadByteReader.readUInt32le
        let reg_count <- MonadByteReader.readUInt32le
        let tot_combo_backs <- MonadByteReader.readUInt32le
        pure {
          taps,
          combo_taps,
          combo_begin,
          group_begin,
          combos_count,
          reg_count,
          tot_combo_backs
        }

def TapSet.tapSize (self: TapSet): Nat := self.group_begin[REGISTER_GROUPS.size]!.toNat

def TapSet.tapIter (self: TapSet): TapIter := {
  iter_data := self.taps.toSubarray
  iter_cursor := 0
  iter_end := self.group_begin[REGISTER_GROUPS.size]!.toNat
}

def TapSet.regIter (self: TapSet): RegIter := {
  iter_data := self.taps.toSubarray
  iter_cursor := 0
  iter_end := self.group_begin[REGISTER_GROUPS.size]!.toNat
}

def TapSet.groupSize (self: TapSet) (group: RegisterGroup): Nat :=
  let group_id := RegisterGroup.toNat group
  let idx := (self.group_begin[group_id + 1]! - 1).toNat
  let last := self.taps[idx]!.offset.toNat
  last + 1

def TapSet.groupTapIter (self: TapSet) (group: RegisterGroup): TapIter :=
  let group_id := RegisterGroup.toNat group
  {
    iter_data := self.taps.toSubarray
    iter_cursor := self.group_begin[group_id]!.toNat
    iter_end := self.group_begin[group_id + 1]!.toNat
  }

def TapSet.groupRegIter (self: TapSet) (group: RegisterGroup): RegIter :=
  let group_id := RegisterGroup.toNat group
  {
    iter_data := self.taps.toSubarray
    iter_cursor := self.group_begin[group_id]!.toNat
    iter_end := self.group_begin[group_id + 1]!.toNat
  }

def TapSet.combosIter (self: TapSet): ComboIter := {
  iter_data := {
    taps := self.combo_taps.toSubarray
    offsets := self.combo_begin.toSubarray
  }
  iter_id := 0
  iter_end := self.combos_count.toNat
}

def TapSet.getCombo (self: TapSet) (id: Nat): ComboRef := {
  data := {
    taps := self.combo_taps.toSubarray
    offsets := self.combo_begin.toSubarray
  }
  id
}


/- TapsProvider -/

class TapsProvider (C: Type) where
  taps: C -> TapSet

end Zkvm.Taps
