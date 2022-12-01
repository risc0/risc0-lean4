/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import R0sy.Lean.UInt32

namespace R0sy.Lean.ByteArray

open R0sy.Lean.UInt32

/- Endian helpers -/

partial def ByteArray.to_be32 (x: ByteArray) (i: Nat := 0) (out: Array UInt32 := #[]): Array UInt32 :=
  if i + 4 <= x.size
  then ByteArray.to_be32 x (i + 4) (out.push (UInt32.of_be32 x[i]! x[i+1]! x[i+2]! x[i+3]!))
  else out

partial def ByteArray.to_le32 (x: ByteArray) (i: Nat := 0) (out: Array UInt32 := #[]): Array UInt32 :=
  if i + 4 <= x.size
  then ByteArray.to_le32 x (i + 4) (out.push (UInt32.of_be32 x[i+3]! x[i+2]! x[i+1]! x[i]!))
  else out

partial def ByteArray.of_le32 (x: Subarray UInt32) (i: Nat := 0) (out: ByteArray := ByteArray.mkEmpty (x.size * 4)): ByteArray
  := if h: i < x.size
      then ByteArray.of_le32 x (i + 1) (out ++ UInt32.to_le x[i])
      else out

#eval (ByteArray.of_le32 #[0xff000001, 0xcc000002].toSubarray).data == #[1, 0, 0, 0xff, 2, 0, 0, 0xcc]
#eval ByteArray.to_le32 (ByteArray.of_le32 #[0xff000001, 0xcc000002].toSubarray) == #[0xff000001, 0xcc000002]

partial def ByteArray.of_be32 (x: Subarray UInt32) (i: Nat := 0) (out: ByteArray := ByteArray.mkEmpty (x.size * 4)): ByteArray
  := if h: i < x.size
      then ByteArray.of_be32 x (i + 1) (out ++ UInt32.to_be x[i])
      else out

end R0sy.Lean.ByteArray
