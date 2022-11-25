/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Ext.UInt32

namespace R0sy.Ext.ByteArray

open R0sy.Ext.UInt32

/- Endian helpers -/

partial def ByteArray.to_be32 (x: ByteArray) (i: Nat := 0) (out: Array UInt32 := #[]): Array UInt32 :=
  if i + 4 <= x.size
  then ByteArray.to_be32 x (i + 4) (out.push (UInt32.of_be32 x[i]! x[i+1]! x[i+2]! x[i+3]!))
  else out

end R0sy.Ext.ByteArray
