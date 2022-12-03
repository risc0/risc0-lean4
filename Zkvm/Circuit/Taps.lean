/-
Copyright (c) 2022 RISC Zero. All rights reserved.
-/

import Zkvm.Taps

namespace Zkvm.Circuit.Taps

open Zkvm.Taps

def TAPS: Array TapData := #[
  {
    offset := 0
    back := 0
    group := RegisterGroup.Accum
    combo := 1
    skip := 2
  },
  {
    offset := 0
    back := 1
    group := RegisterGroup.Accum
    combo := 1
    skip := 2
  },
  {
    offset := 1
    back := 0
    group := RegisterGroup.Accum
    combo := 1
    skip := 2
  },
  {
    offset := 1
    back := 1
    group := RegisterGroup.Accum
    combo := 1
    skip := 2
  },
  {
    offset := 2
    back := 0
    group := RegisterGroup.Accum
    combo := 1
    skip := 2
  },
  {
    offset := 2
    back := 1
    group := RegisterGroup.Accum
    combo := 1
    skip := 2
  },
  {
    offset := 3
    back := 0
    group := RegisterGroup.Accum
    combo := 1
    skip := 2
  },
  {
    offset := 3
    back := 1
    group := RegisterGroup.Accum
    combo := 1
    skip := 2
  },
  {
    offset := 4
    back := 0
    group := RegisterGroup.Accum
    combo := 1
    skip := 2
  },
  {
    offset := 4
    back := 1
    group := RegisterGroup.Accum
    combo := 1
    skip := 2
  },
  {
    offset := 5
    back := 0
    group := RegisterGroup.Accum
    combo := 1
    skip := 2
  },
  {
    offset := 5
    back := 1
    group := RegisterGroup.Accum
    combo := 1
    skip := 2
  },
  {
    offset := 6
    back := 0
    group := RegisterGroup.Accum
    combo := 1
    skip := 2
  },
  {
    offset := 6
    back := 1
    group := RegisterGroup.Accum
    combo := 1
    skip := 2
  },
  {
    offset := 7
    back := 0
    group := RegisterGroup.Accum
    combo := 1
    skip := 2
  },
  {
    offset := 7
    back := 1
    group := RegisterGroup.Accum
    combo := 1
    skip := 2
  },
  {
    offset := 8
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 9
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 10
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 11
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 12
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 13
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 14
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 15
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 16
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 17
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 18
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 19
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 20
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 21
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 22
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 23
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 24
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 25
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 26
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 27
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 28
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 29
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 30
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 31
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 32
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 33
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 34
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 35
    back := 0
    group := RegisterGroup.Accum
    combo := 0
    skip := 1
  },
  {
    offset := 0
    back := 0
    group := RegisterGroup.Code
    combo := 0
    skip := 1
  },
  {
    offset := 1
    back := 0
    group := RegisterGroup.Code
    combo := 0
    skip := 1
  },
  {
    offset := 2
    back := 0
    group := RegisterGroup.Code
    combo := 1
    skip := 2
  },
  {
    offset := 2
    back := 1
    group := RegisterGroup.Code
    combo := 1
    skip := 2
  },
  {
    offset := 3
    back := 0
    group := RegisterGroup.Code
    combo := 0
    skip := 1
  },
  {
    offset := 4
    back := 0
    group := RegisterGroup.Code
    combo := 0
    skip := 1
  },
  {
    offset := 5
    back := 0
    group := RegisterGroup.Code
    combo := 0
    skip := 1
  },
  {
    offset := 6
    back := 0
    group := RegisterGroup.Code
    combo := 0
    skip := 1
  },
  {
    offset := 7
    back := 0
    group := RegisterGroup.Code
    combo := 0
    skip := 1
  },
  {
    offset := 8
    back := 0
    group := RegisterGroup.Code
    combo := 0
    skip := 1
  },
  {
    offset := 9
    back := 0
    group := RegisterGroup.Code
    combo := 0
    skip := 1
  },
  {
    offset := 10
    back := 0
    group := RegisterGroup.Code
    combo := 0
    skip := 1
  },
  {
    offset := 11
    back := 0
    group := RegisterGroup.Code
    combo := 0
    skip := 1
  },
  {
    offset := 12
    back := 0
    group := RegisterGroup.Code
    combo := 0
    skip := 1
  },
  {
    offset := 13
    back := 0
    group := RegisterGroup.Code
    combo := 0
    skip := 1
  },
  {
    offset := 14
    back := 0
    group := RegisterGroup.Code
    combo := 0
    skip := 1
  },
  {
    offset := 0
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 0
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 1
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 1
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 2
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 2
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 3
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 3
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 4
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 4
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 5
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 5
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 6
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 6
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 7
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 7
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 8
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 8
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 9
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 10
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 10
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 11
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 11
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 12
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 12
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 13
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 14
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 15
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 16
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 17
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 18
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 19
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 20
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 20
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 20
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 20
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 20
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 21
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 21
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 21
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 21
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 21
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 22
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 22
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 22
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 22
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 22
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 23
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 23
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 23
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 23
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 23
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 24
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 24
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 24
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 24
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 24
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 25
    back := 0
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 25
    back := 1
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 25
    back := 2
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 25
    back := 7
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 25
    back := 15
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 25
    back := 16
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 26
    back := 0
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 26
    back := 1
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 26
    back := 2
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 26
    back := 7
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 26
    back := 15
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 26
    back := 16
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 27
    back := 0
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 27
    back := 1
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 27
    back := 2
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 27
    back := 7
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 27
    back := 15
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 27
    back := 16
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 28
    back := 0
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 28
    back := 1
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 28
    back := 2
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 28
    back := 7
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 28
    back := 15
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 28
    back := 16
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 29
    back := 0
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 29
    back := 1
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 29
    back := 2
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 29
    back := 7
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 29
    back := 15
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 29
    back := 16
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 30
    back := 0
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 30
    back := 1
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 30
    back := 2
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 30
    back := 7
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 30
    back := 15
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 30
    back := 16
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 31
    back := 0
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 31
    back := 1
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 31
    back := 2
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 31
    back := 7
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 31
    back := 15
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 31
    back := 16
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 32
    back := 0
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 32
    back := 1
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 32
    back := 2
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 32
    back := 7
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 32
    back := 15
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 32
    back := 16
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 33
    back := 0
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 33
    back := 1
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 33
    back := 2
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 33
    back := 7
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 33
    back := 15
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 33
    back := 16
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 34
    back := 0
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 34
    back := 1
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 34
    back := 2
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 34
    back := 7
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 34
    back := 15
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 34
    back := 16
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 35
    back := 0
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 35
    back := 1
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 35
    back := 2
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 35
    back := 7
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 35
    back := 15
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 35
    back := 16
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 36
    back := 0
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 36
    back := 1
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 36
    back := 2
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 36
    back := 7
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 36
    back := 15
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 36
    back := 16
    group := RegisterGroup.Data
    combo := 4
    skip := 6
  },
  {
    offset := 37
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 37
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 37
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 37
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 37
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 38
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 38
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 38
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 38
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 38
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 39
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 39
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 39
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 39
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 39
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 40
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 40
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 40
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 40
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 40
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 41
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 41
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 41
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 41
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 41
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 42
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 43
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 44
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 45
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 46
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 47
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 48
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 49
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 50
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 50
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 51
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 51
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 52
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 53
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 54
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 55
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 56
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 57
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 58
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 59
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 60
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 61
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 62
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 63
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 64
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 65
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 66
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 67
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 68
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 69
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 70
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 71
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 72
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 72
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 73
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 73
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 74
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 75
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 76
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 77
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 78
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 79
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 80
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 81
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 82
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 82
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 82
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 82
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 82
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 83
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 83
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 83
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 83
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 83
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 84
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 84
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 84
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 84
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 84
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 85
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 85
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 85
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 85
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 85
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 86
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 86
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 86
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 86
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 86
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 87
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 87
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 87
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 87
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 87
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 88
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 88
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 88
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 88
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 88
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 89
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 89
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 89
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 89
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 89
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 90
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 90
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 90
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 90
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 90
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 91
    back := 0
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 91
    back := 2
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 91
    back := 7
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 91
    back := 15
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 91
    back := 16
    group := RegisterGroup.Data
    combo := 5
    skip := 5
  },
  {
    offset := 92
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 93
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 94
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 95
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 95
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 96
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 96
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 97
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 97
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 98
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 98
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 99
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 99
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 100
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 100
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 101
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 101
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 102
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 102
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 103
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 103
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 104
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 104
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 105
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 105
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 106
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 106
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 107
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 108
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 109
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 110
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 111
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 112
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 113
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 114
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 115
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 116
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 117
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 117
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 118
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 118
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 119
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 119
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 120
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 120
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 121
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 122
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 123
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 124
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 124
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 125
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 125
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 126
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 126
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 127
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 127
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 128
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 129
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 130
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 130
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 131
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 131
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 132
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 132
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 133
    back := 0
    group := RegisterGroup.Data
    combo := 2
    skip := 3
  },
  {
    offset := 133
    back := 1
    group := RegisterGroup.Data
    combo := 2
    skip := 3
  },
  {
    offset := 133
    back := 2
    group := RegisterGroup.Data
    combo := 2
    skip := 3
  },
  {
    offset := 134
    back := 0
    group := RegisterGroup.Data
    combo := 2
    skip := 3
  },
  {
    offset := 134
    back := 1
    group := RegisterGroup.Data
    combo := 2
    skip := 3
  },
  {
    offset := 134
    back := 2
    group := RegisterGroup.Data
    combo := 2
    skip := 3
  },
  {
    offset := 135
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 136
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 137
    back := 0
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 137
    back := 1
    group := RegisterGroup.Data
    combo := 1
    skip := 2
  },
  {
    offset := 138
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 139
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 140
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 141
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 142
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 143
    back := 0
    group := RegisterGroup.Data
    combo := 0
    skip := 1
  },
  {
    offset := 144
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 144
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 144
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 144
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 144
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 144
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 145
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 145
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 145
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 145
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 145
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 145
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 146
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 146
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 146
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 146
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 146
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 146
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 147
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 147
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 147
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 147
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 147
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 147
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 148
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 148
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 148
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 148
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 148
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 148
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 149
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 149
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 149
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 149
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 149
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 149
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 150
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 150
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 150
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 150
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 150
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 150
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 151
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 151
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 151
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 151
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 151
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 151
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 152
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 152
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 152
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 152
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 152
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 152
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 153
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 153
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 153
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 153
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 153
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 153
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 154
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 154
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 154
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 154
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 154
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 154
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 155
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 155
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 155
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 155
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 155
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 155
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 156
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 156
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 156
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 156
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 156
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 156
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 157
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 157
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 157
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 157
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 157
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 157
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 158
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 158
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 158
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 158
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 158
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 158
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 159
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 159
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 159
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 159
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 159
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 159
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 160
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 160
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 160
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 160
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 160
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 160
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 161
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 161
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 161
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 161
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 161
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 161
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 162
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 162
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 162
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 162
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 162
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 162
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 163
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 163
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 163
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 163
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 163
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 163
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 164
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 164
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 164
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 164
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 164
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 164
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 165
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 165
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 165
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 165
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 165
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 165
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 166
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 166
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 166
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 166
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 166
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 166
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 167
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 167
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 167
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 167
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 167
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 167
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 168
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 168
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 168
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 168
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 168
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 168
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 169
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 169
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 169
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 169
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 169
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 169
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 170
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 170
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 170
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 170
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 170
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 170
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 171
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 171
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 171
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 171
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 171
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 171
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 172
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 172
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 172
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 172
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 172
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 172
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 173
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 173
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 173
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 173
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 173
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 173
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 174
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 174
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 174
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 174
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 174
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 174
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 175
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 175
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 175
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 175
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 175
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 175
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 176
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 176
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 176
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 176
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 176
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 176
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 177
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 177
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 177
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 177
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 177
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 177
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 178
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 178
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 178
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 178
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 178
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 178
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 179
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 179
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 179
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 179
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 179
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 179
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 180
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 180
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 180
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 180
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 180
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 180
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 181
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 181
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 181
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 181
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 181
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 181
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 182
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 182
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 182
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 182
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 182
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 182
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 183
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 183
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 183
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 183
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 183
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 183
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 184
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 184
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 184
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 184
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 184
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 184
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 185
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 185
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 185
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 185
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 185
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 185
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 186
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 186
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 186
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 186
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 186
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 186
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 187
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 187
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 187
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 187
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 187
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 187
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 188
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 188
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 188
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 188
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 188
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 188
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 189
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 189
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 189
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 189
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 189
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 189
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 190
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 190
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 190
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 190
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 190
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 190
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 191
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 191
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 191
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 191
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 191
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 191
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 192
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 192
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 192
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 192
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 192
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 192
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 193
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 193
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 193
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 193
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 193
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 193
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 194
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 194
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 194
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 194
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 194
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 194
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 195
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 195
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 195
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 195
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 195
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 195
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 196
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 196
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 196
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 196
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 196
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 196
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 197
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 197
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 197
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 197
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 197
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 197
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 198
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 198
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 198
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 198
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 198
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 198
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 199
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 199
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 199
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 199
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 199
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 199
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 200
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 200
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 200
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 200
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 200
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 200
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 201
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 201
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 201
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 201
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 201
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 201
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 202
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 202
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 202
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 202
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 202
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 202
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 203
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 203
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 203
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 203
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 203
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 203
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 204
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 204
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 204
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 204
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 204
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 204
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 205
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 205
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 205
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 205
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 205
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 205
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 206
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 206
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 206
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 206
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 206
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 206
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 207
    back := 0
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 207
    back := 1
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 207
    back := 2
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 207
    back := 3
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 207
    back := 4
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  },
  {
    offset := 207
    back := 68
    group := RegisterGroup.Data
    combo := 3
    skip := 6
  }
]

def TAPSET: TapSet := {
  taps := TAPS,
  combo_taps := #[
    0,  0,  1, 0,  1, 2,  0, 1,
    2,  3,  4, 68, 0, 1,  2, 7,
    15, 16, 0, 2,  7, 15, 16
  ],
  combo_begin := #[0, 1, 3, 6, 12, 18, 23],
  group_begin := #[0, 44, 60, 772],
  combos_count := 6,
  reg_count := 259,
  tot_combo_backs := 23,
}

end Zkvm.Circuit.Taps
