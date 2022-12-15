/- Transcription of risczero-wip/blob/main/cirgen/compiler/IR/Ops.td -/

import Cirgen.Compiler.IR.Types

/- ******************************** From Ops.td  ********************** -/
section CirgenOp_Syntax

variable (isz neg inv add sub mul bitAnd:String)

/- In cirgen, there is a separate file Attrs.h that defines a class for this-/
inductive PolynomialAttr where
| poly: Array UInt64 -> PolynomialAttr

def I32Attr: Type := Int /- i.e. a rbitrary-precision integers. There are no overflows. TODO: CPP uses int32 -/

def UI32Attr: Type := UInt32 /-used in SliceOp-/

/-variable (StrAttr:Type)  used in ExternOp, LookupOp etc-/
def StrAttr: Type := String

def BufferUnit := Buffer Unit /- TODO: replace Buffer Unit with Buffer T for some sensible T -- maybe the field? see Types.lean-/

def BufferMLIRVal := Buffer MLIR_Value /- TODO: replace Buffer Unit with Buffer T for some sensible T -- maybe the field? see Types.lean-/

def RefUnit := Ref Unit /- TODO: replace Ref Unit with Ref T for some sensible T -- maybe the field? see Types.lean-/

def HashOpArgs := Unit /- TODO: decide how to model variadic type -/

def IOPReadOpArgs := Unit /- TODO: decide how to model variadic type -/
def SelectOpArgs := Unit /- TODO: decide how to model variadic type -/

inductive CirgenOp where
| ConstOp: Poly -> CirgenOp
| NondetOp: CirgenOp
| IfOp: Val -> CirgenOp -> CirgenOp /- I believe the fact that Ops.td.IfOp has an inner region indicates existence of a then-branch, and that else-branch is empty-/
| AllocOp: CirgenOp
| BackOp: BufferUnit -> I32Attr -> CirgenOp 
| SliceOp: BufferUnit -> UI32Attr -> UI32Attr -> CirgenOp
| GetOp: BufferUnit -> UI32Attr -> UI32Attr -> (Option UI32Attr) -> CirgenOp
| SetOp: BufferMLIRVal -> UI32Attr -> Val -> CirgenOp
| GetGlobalOp: BufferUnit -> UI32Attr -> CirgenOp
| SetGlobalOp: BufferUnit -> UI32Attr -> Val -> CirgenOp
| EqualZeroOp: Val -> CirgenOp
| BarrierOp: Val -> CirgenOp
| PowOp: Val -> UI32Attr -> CirgenOp
| UnaryOp: String -> Val -> CirgenOp /-first argument identifies the unary operator, eg UnaryOp "isz" or UniaryOp "inv"; see Ops.td-/
| BinaryOp: String -> Val -> Val -> CirgenOp 
| TerminateOp: CirgenOp
| FailOp: CirgenOp
| TrueOp: CirgenOp
| AndEqZOp: Constraint -> Val -> CirgenOp
| AndCondp: Constraint -> Val -> Constraint -> CirgenOp
| ExternOp: List Val -> StrAttr -> StrAttr -> CirgenOp
| LookupOp: ContainerType -> StrAttr -> CirgenOp
| SubscriptOp: ArrayType -> UI32Attr -> CirgenOp
| LoadOp: RefUnit -> CirgenOp /-TODO: better instantiation of VAL parameter of Ref-/
| StoreOp: RefUnit -> Val -> CirgenOp
| HashOp: HashOpArgs -> CirgenOp
| HashFoldOp: Digest -> Digest -> CirgenOp

/-| HashAssertEqOp: Digest -> Digest -> CirgenOp wrong: eval accesses inter.
  or HashAssertEqOp: Val -> Val -> CirgenOp
  or HashAssertEqOp: MLIR_Value -> MLIR_Value -> CirgenOp ??? -/

| IOPReadOp: IOPReadOpArgs -> CirgenOp
| IOPCommitOp: IOP -> Digest -> CirgenOp
| IOPRngBitsOp: IOP -> UI32Attr-> CirgenOp
| IOPRngValOp: IOP -> CirgenOp
| SelectOp: SelectOpArgs -> CirgenOp

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