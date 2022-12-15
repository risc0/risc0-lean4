import Cirgen.Compiler.IR.Types
import Cirgen.Compiler.IR.Ops

def Map (A B:Type) [DecidableEq A]:= A -> Option B

def upd A B [DecidableEq A](M:Map A B) a b :=
  fun a' => if a=a' then b else M a'

def emptyMap (A B:Type) [DecidableEq A]: Map A B :=
  fun _ => none

section interpreter
partial def ValNeg: Val -> Val := sorry /- Negation on the extension field -/
partial def ValInv: Val -> Val := sorry /- Inversion on the extension field -/
partial def ValAdd: Val -> Val -> Val := sorry /- Addition on the extension field -/
partial def ValSub: Val -> Val -> Val := sorry /- Subtraction on the extension field -/
partial def ValMul: Val -> Val -> Val := sorry /- Multiplication on the extension field -/
partial def ValBitAnd: Val -> Val -> Val := sorry /- Bitwise And on the extension field -/

def EvalIsZeroOp (v:Val): Bool := if v.fieldP == 0 then true else false
def EvalNegOp (v:Val): Val := ValNeg v.fieldP
def EvalInvOp (v:Val): Val := ValInv v.fieldP
def EvalAddOp (u v:Val): Val := ValAdd u.fieldP v.fieldP
def EvalSubOp (u v:Val): Val := ValSub u.fieldP v.fieldP
def EvalMulOp (u v:Val): Val := ValMul u.fieldP v.fieldP
def EvalBitAndOp (u v:Val): Val := ValBitAnd u.fieldP v.fieldP

def size_t:Type := UInt32 /- in principle, this should be USize, but we're targetting RiscV32. TODO: maybe eliminate this type? -/ 

def size_t_zero:size_t := UInt32.ofNatCore 0 (by decide) /- USize.ofNatCore 0 (by decide) -/
def size_t_one:size_t := UInt32.ofNatCore 1 (by decide) /- USize.ofNatCore 1 (by decide) -/

/-Moved tp Types.lean:  def MLIR_Value := Unit  -/

variable [MLIR_Value_dec: DecidableEq MLIR_Value]

def Polynomial := Array UInt64 /- size = 4 not modeled. Alternative: use uint64 quadruple  -/

/-  def PolynomialRef := Polynomial;Rust: Ref to an array of unit64, so unify with Polynomial -/

/-  Can/should we unify this with Types.Buffer?  -/
def InterpreterBuffer := Array Polynomial /-  Rust: Vector of unspecified size  -/
/-  def BufferRef := InterpreterBuffer; Rust: Ref to a mutable array of polynomials, so unify with InterpreterBuffer -/

structure ExternHandler where
  name: String
  extra:String
  arg: Array UInt64
  outCount: size_t

def dummyHandler:ExternHandler := sorry
 /-  needed in lieu of nullptr in constructor of interpreter
  TODO: see whether we can eliminate this -/

def ReadIOPImpl := Unit /-  TODO: replace with meangful type  -/
def ShaDigest := Unit /-  TODO: replace with meangful type  -/

structure InterpreterRep where
  cycle: size_t/-  := size_t_zero -/
  handler: ExternHandler /-  := dummyHandler -/
  vals: Map MLIR_Value Polynomial /-  MLIR_Value_dec := emptyMap -/
  bufs: Map MLIR_Value InterpreterBuffer /- MLIR_Value_dec := emptyMap -/
  iops: Map MLIR_Value ReadIOPImpl /-  MLIR_Value_dec := emptyMap -/
  digests: Map MLIR_Value ShaDigest /- MLIR_Value_dec := emptyMap -/

def setCycle [DecidableEq MLIR_Value] (I:InterpreterRep /- MLIR_Value Polynomial -/) (c:size_t)
    :InterpreterRep /- MLIR_Value Polynomial -/ := 
    { I with cycle := c }
   /- InterpreterRep.mk c I.handler I.vals I.bufs -/

def getCycle (I:InterpreterRep /- MLIR_Value MLIR_Value_dec -/): size_t := I.cycle

def setExternHandler (I:InterpreterRep) (h:ExternHandler):InterpreterRep := 
   { I with handler := h }

def getExternHandler (I:InterpreterRep ): ExternHandler := I.handler

def setVal (I:InterpreterRep) (value:MLIR_Value)(poly:Polynomial):InterpreterRep := 
   { I with vals := upd _ _ I.vals value poly } /- CPP code refers to poly.begin() and poly.end() which I ssustes yields the array describing the coeffs -/

def getVal (I:InterpreterRep) (value: MLIR_Value):Option Polynomial := I.vals value

def setBuf (I:InterpreterRep) (buffer: MLIR_Value)(val:InterpreterBuffer):InterpreterRep  := 
   { I with bufs := upd _ _ I.bufs buffer val}

variable (mkNewPoly: size_t -> size_t -> Polynomial) /- eg newPoly (1,size)  -/
variable (newInterpreterBuffer: InterpreterBuffer)

/- TODO: maye use monad? -/
/- TODO: correct/fill in how new poly is converted into newbuff vector of vecot of polynomials in CPP? -/
def makeBuf (I:InterpreterRep) (buffer: MLIR_Value)
           (size:size_t)(kind:BufferKind): InterpreterRep × Polynomial :=
           let newPoly := mkNewPoly size_t_one size
           let newbuff :=newInterpreterBuffer
           let newbuffs := upd MLIR_Value InterpreterBuffer I.bufs buffer newbuff
           let Inew := { I with bufs := newbuffs } 
           (Inew, newPoly)

def getBuf (I:InterpreterRep)(buffer: MLIR_Value): Option InterpreterBuffer := I.bufs buffer

def setIOP (I:InterpreterRep)(value: MLIR_Value) (iop: ReadIOPImpl): InterpreterRep := 
  { I with iops := upd _ _ I.iops value iop }

def getIOP (I:InterpreterRep)(value: MLIR_Value): Option ReadIOPImpl := I.iops value

def setDigest (I:InterpreterRep)(value: MLIR_Value)(digest: ShaDigest): InterpreterRep :=
 { I with digests := upd _ _ I.digests value digest }

def getDigest (I:InterpreterRep)(value: MLIR_Value): Option ShaDigest := I.digests value

variable (DummyVal:Val)

partial def step (I:InterpreterRep) (op:CirgenOp): Option (InterpreterRep × Val) :=
  match op with
| CirgenOp.ConstOp P => none
| CirgenOp.NondetOp => none
| CirgenOp.IfOp cond e => if !(EvalIsZeroOp cond) then step I e else none 
   /- TODO: else branch empty: should we return some (I, somdefaultVal)? -/
/-
| AllocOp: CirgenOp
| BackOp: BufferUnit -> I32Attr -> CirgenOp 
| SliceOp: BufferUnit -> UI32Attr -> UI32Attr -> CirgenOp
| GetOp: BufferUnit -> UI32Attr -> UI32Attr -> (Option UI32Attr) -> CirgenOp -/
| CirgenOp.SetOp buf offset v =>
      match getBuf I (Buffer.element buf), getVal I v with
      | some vec, some newval  => /- TODO: check that dyn.static intuition is correct, corretc Unit32 versus Nat, and 
                                     and understand the Error condition in the CPP code-/
                  let (dynSize:Nat) := vec.size
                  let (szOffset:UInt32) := UInt32.mul dynSize (getCycle I)
                  let (totalOffset:UInt32) := UInt32.add szOffset offset
                  let (staticSize:UInt32) := Buffer.size buf
                  if UInt32.val totalOffset >= staticSize then panic!() else
                  let val := vec[totalOffset]
                  vec[totalOffset] := newval
                  let newI := setBuf I (Buffer.element buf) vec
                  some (newI, DummyVal)
      | _, _ => none

/- | GetGlobalOp: BufferUnit -> UI32Attr -> CirgenOp
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
| HashAssertEqOp: Digest -> Digest -> CirgenOp
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
-/
| _ => none

end interpreter