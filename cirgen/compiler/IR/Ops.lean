/- Transcription of risczero-wip/blob/main/cirgen/compiler/IR/Ops.td -/

import cirgen
import cirgen.compiler
import cirgen.compiler.IR
import cirgen.compiler.IR.Types

/- ******************************** From Ops.td  ********************** -/
section CirgenOp_Syntax

variable (String:Type) /-used to name operations; could use builtin type,
           or define inductive types for unary and binary operators-/

variable (isz neg inv add sub mul bitAnd:String)

/- In cirgen, there is a separate file Attrs.h that defines a class for this-/
inductive PolynomialAttr where
  poly: Array uint64 -> PolynomialAttr

variable (Val:Type)
/- Val stands for the field. Or maybe the extension field... -/

variable (Buffer:Type) /-used in BackOp -- probably comes from Types.lean-/

variable (Constraint:Type) /-used in AndEqzOp, AndCondOp etc. Probably comes from Types.lean-/

def I32Attr:Type := int32 /-used in BackOp -- maybe just Z? -/

def UI32Attr :Type := unit32 /-used in SliceOp-/

/-variable (StrAttr:Type)  used in ExternOp, LookupOp etc-/
def StrAttr:Type := String

variable (ContainerType:Type) /-refine to Variant StructType | UnionType? used in LookupOp/ etc. See types.h etc-/

variable (ArrayType:Type) /-Maybe parametrize by Type of elements? Used in SubscriptOp-/

variable (Ref:Type)  /-member reference, eg in structs etc it seems; used in LoadOp and StoreOp-/

inductive CirgenOp where
| ConstOp: Poly -> CirgenOp
| NondetOp: CirgenOp
| IfOp: Val -> CirgenOp
| AllocOp: CirgenOp
| BackOp: Buffer -> I32Attr -> CirgenOp
| SliceOp: Buffer -> UI32Attr -> UI32Attr -> CirgenOp
| GetOp: Buffer -> UI32Attr -> UI32Attr -> (Option UI32Attr) -> CirgenOp
| SetOp: Buffer -> UI32Attr -> Val -> CirgenOp
| GetGlobalOp: Buffer -> UI32Attr -> CirgenOp
| SetGlobalOp: Buffer -> UI32Attr -> Val -> CirgenOp
| EqualZeroOp: Val -> CirgenOp
| BarrierOp: Val -> CirgenOp
| PowOp: Val -> UI32Attr -> CirgenOp
| UnaryOp: String -> Val -> CirgenOp /-first argument identifiers the unary operator, eg UnaryOp "isz" or UniaryOp "inv"; see Ops.td-/
| BinaryOp: String -> Val -> Val -> CirgenOp 
| TerminateOp: CirgenOp
| FailOp: CirgenOp
| TrueOp: CirgenOp
| AndEqZOp: Constraint -> Val -> CirgenOp
| ExternOp: List Val -> StrAttr -> StrAttr -> CirgenOp
| LookupOp: ContainerType -> StrAttr -> CirgenOp
| SubscriptOp: ArrayType -> UI32Attr -> CirgenOp
| LoadOp: Ref -> CirgenOp
| StoreOp: Ref -> Val -> CirgenOp

/-TODO: fill in def, once we have decided poly representation-/
variable (ConstOpBuilder: uint64 -> Poly)

def IsZeroOp := CirgenOp.UnaryOp isz
def NegOp := UnaryOp neg
def InvOp := UnaryOp inv

def AddOp := BinaryOp add
def SubOp := BinaryOp sub
def MulOp := BinaryOp mul
def BitAndOp := BinaryOp bitAnd

end CirgenOp_Syntax