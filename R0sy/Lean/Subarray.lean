/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

namespace R0sy.Lean.Subarray

def Subarray.take (x: Subarray X) (n: Nat): Subarray X × Subarray X :=
  let xx := x.as
  let l := xx.toSubarray x.start (x.start + n)
  let r := xx.toSubarray (x.start + n) (x.stop)
  (l, r)

end R0sy.Lean.Subarray
