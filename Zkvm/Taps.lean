/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace Zkvm.Taps

inductive RegisterGroup where
  | Accum
  | Code
  | Data

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


structure TapData where
  offset: UInt16
  back: UInt16
  group: RegisterGroup
  combo: UInt8
  skip: UInt8


structure TapSet where
  taps: Array TapData
  combo_taps: Array UInt16
  combo_begin: Array UInt16
  group_begin: Array USize
  combos_count: USize
  reg_count: USize
  tot_combo_backs: USize


structure TapIter where
  -- TODO

structure RegIter where
  -- TODO

structure ComboIter where
  -- TODO

structure ComboRef where
  -- TODO

def TapSet.tapSize (self: TapSet): USize := self.group_begin[REGISTER_GROUPS.size]!

def TapSet.tapIter (self: TapSet): TapIter := sorry

def TapSet.regIter (self: TapSet): RegIter := sorry

def TapSet.groupSize (self: TapSet) (group: RegisterGroup): USize := sorry

def TapSet.groupTapIter (self: TapSet) (group: RegisterGroup): TapIter := sorry

def TapSet.groupRegIter (self: TapSet) (group: RegisterGroup): RegIter := sorry

def TapSet.combosIter (self: TapSet): ComboIter := sorry

def TapSet.getCombo (self: TapSet) (id: USize): ComboRef := sorry

class TapsProvider (C: Type) where
  taps: C -> TapSet

end Zkvm.Taps
