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


/- RegIter -/

structure RegIter where
  iter_data: Subarray TapData
  iter_cursor: Nat
  iter_end: Nat


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
