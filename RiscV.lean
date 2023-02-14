/-
 Copyright 2023 RISC Zero, Inc.

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
-/

import RiscV.Config
import RiscV.Elf
import RiscV.Instr.ISA
import RiscV.Instr.RV32I
import RiscV.Instr.RV32M
import RiscV.Instr.Types
import RiscV.ISA
import RiscV.Mach.Exception
import RiscV.Mach.Int
import RiscV.Mach.Mem
import RiscV.Mach.Reg
import RiscV.Mach.XlenInt
import RiscV.Monad

/-!
# RISC-V

The RISC-V instruction set architecture.
-/
