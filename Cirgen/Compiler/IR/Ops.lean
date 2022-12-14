/- Transcription of risczero-wip/blob/main/cirgen/compiler/IR/Ops.td -/

import Cirgen.Compiler.IR.Types

/- ******************************** From Ops.td  ********************** -/
section CirgenOp_Syntax

variable (isz neg inv add sub mul bitAnd:String)

/- In cirgen, there is a separate file Attrs.h that defines a class for this-/
inductive PolynomialAttr where
| poly: Array UInt64 -> PolynomialAttr

def I32Attr: Type := Int /- TODO: int32 ; used in BackOp -- maybe just Z? -/

def UI32Attr: Type := UInt32 /-used in SliceOp-/

/-variable (StrAttr:Type)  used in ExternOp, LookupOp etc-/
def StrAttr: Type := String

def BufferUnit := Buffer Unit /- TODO: replace Buffer Unit with Buffer T for some sensible T -- maybe the field? see Types.lean-/

def RefrUnit := Ref Unit /- TODO: replace Ref Unit with Ref T for some sensible T -- maybe the field? see Types.lean-/

inductive CirgenOp where
| ConstOp: Poly -> CirgenOp
| NondetOp: CirgenOp
| IfOp: Val -> CirgenOp
| AllocOp: CirgenOp
| BackOp: BufferUnit -> I32Attr -> CirgenOp 
| SliceOp: BufferUnit -> UI32Attr -> UI32Attr -> CirgenOp
| GetOp: BufferUnit -> UI32Attr -> UI32Attr -> (Option UI32Attr) -> CirgenOp
| SetOp: BufferUnit -> UI32Attr -> Val -> CirgenOp
| GetGlobalOp: BufferUnit -> UI32Attr -> CirgenOp
| SetGlobalOp: BufferUnit -> UI32Attr -> Val -> CirgenOp
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
| LoadOp: RefUnit -> CirgenOp /-TODO: better instantiation of VAL parameter of Ref-/
| StoreOp: RefUnit -> Val -> CirgenOp

/-TODO: fill in def, once we have decided poly representation-/
variable (ConstOpBuilder: uint64 -> Poly)

def IsZeroOp := CirgenOp.UnaryOp isz
def NegOp := CirgenOp.UnaryOp neg
def InvOp := CirgenOp.UnaryOp inv

def AddOp := CirgenOp.BinaryOp add
def SubOp := CirgenOp.BinaryOp sub
def MulOp := CirgenOp.BinaryOp mul
def BitAndOp := CirgenOp.BinaryOp bitAnd

end CirgenOp_Syntax