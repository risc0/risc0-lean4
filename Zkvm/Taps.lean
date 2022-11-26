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
  iter_cursor: USize
  iter_end: USize


/- RegIter -/

structure RegIter where
  iter_data: Subarray TapData
  iter_cursor: USize
  iter_end: USize


/- Combo -/

structure ComboData where
  taps: Subarray UInt16
  offsets: Subarray UInt16


structure ComboIter where
  iter_data: ComboData
  iter_id: USize
  iter_end: USize


structure ComboRef where
  data: ComboData
  id: USize


/- TapSet -/

structure TapSet where
  taps: Array TapData
  combo_taps: Array UInt16
  combo_begin: Array UInt16
  group_begin: Array USize
  combos_count: USize
  reg_count: USize
  tot_combo_backs: USize

def TapSet.tapSize (self: TapSet): USize := self.group_begin[REGISTER_GROUPS.size]!

def TapSet.tapIter (self: TapSet): TapIter := sorry

def TapSet.regIter (self: TapSet): RegIter := sorry

def TapSet.groupSize (self: TapSet) (group: RegisterGroup): USize := sorry

def TapSet.groupTapIter (self: TapSet) (group: RegisterGroup): TapIter := sorry

def TapSet.groupRegIter (self: TapSet) (group: RegisterGroup): RegIter := sorry

def TapSet.combosIter (self: TapSet): ComboIter := sorry

def TapSet.getCombo (self: TapSet) (id: USize): ComboRef := sorry


/- TapsProvider -/

class TapsProvider (C: Type) where
  taps: C -> TapSet

end Zkvm.Taps
