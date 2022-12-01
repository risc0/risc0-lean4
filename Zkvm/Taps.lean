/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace Zkvm.Taps

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
  group_begin: Array Nat
  combos_count: Nat
  reg_count: Nat
  tot_combo_backs: Nat
  deriving Inhabited

def TapSet.tapSize (self: TapSet): Nat := self.group_begin[REGISTER_GROUPS.size]!

def TapSet.tapIter (self: TapSet): TapIter := sorry

def TapSet.regIter (self: TapSet): RegIter := sorry

def TapSet.groupSize (self: TapSet) (group: RegisterGroup): Nat :=
  let group_id := RegisterGroup.toNat group
  let idx := self.group_begin[group_id + 1]! - 1
  let last := self.taps[idx]!.offset.toNat
  last + 1

def TapSet.groupTapIter (self: TapSet) (group: RegisterGroup): TapIter := sorry

def TapSet.groupRegIter (self: TapSet) (group: RegisterGroup): RegIter := sorry

def TapSet.combosIter (self: TapSet): ComboIter := sorry

def TapSet.getCombo (self: TapSet) (id: Nat): ComboRef := sorry


/- TapsProvider -/

class TapsProvider (C: Type) where
  taps: C -> TapSet

end Zkvm.Taps
