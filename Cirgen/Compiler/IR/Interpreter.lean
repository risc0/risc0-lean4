import Cirgen.Compiler.IR.Types
import Cirgen.Compiler.IR.Ops

def Map (A B:Type) [DecidableEq A]:= A -> Option B

def upd A B [DecidableEq A](M:Map A B) a b :=
  fun a' => if a=a' then b else M a'

def emptyMap (A B:Type) [DecidableEq A]: Map A B :=
  fun _ => none

section interpreter
partial def ValNeg: Val -> Val := sorry /-Negation on the extension field-/
partial def ValInv: Val -> Val := sorry /-Inversion on the extension field-/
partial def ValAdd: Val -> Val -> Val := sorry /-Addition on the extension field-/
partial def ValSub: Val -> Val -> Val := sorry /-Subtraction on the extension field-/
partial def ValMul: Val -> Val -> Val := sorry /-Multiplication on the extension field-/
partial def ValBitAnd: Val -> Val -> Val := sorry /-Bitwise And on the extension field-/

def EvalIsZeroOp (v:Val): Bool := if v.fieldP == 0 then true else false
def EvalNegOp (v:Val): Val := ValNeg v.fieldP
def EvalInvOp (v:Val): Val := ValInv v.fieldP
def EvalAddOp (u v:Val): Val := ValAdd u.fieldP v.fieldP
def EvalSubOp (u v:Val): Val := ValSub u.fieldP v.fieldP
def EvalMulOp (u v:Val): Val := ValMul u.fieldP v.fieldP
def EvalBitAndOp (u v:Val): Val := ValBitAnd u.fieldP v.fieldP

def size_t:Type := UInt64 /- TODO: maybe eliminate this type?-/

def size_t_zero:size_t := sorry

partial def size_t_one:size_t := sorry

def MLIR_Value := Unit

variable [MLIR_Value_dec: DecidableEq MLIR_Value]

def Polynomial := Array UInt64 /-size = 4 not modeled. Alternative: use uint64 quadruple -/
/- def PolynomialRef := Polynomial;Rust: Ref to an array of unit64, so unify with Polynomial-/

variable (mkNewPoly: size_t -> size_t -> Polynomial) /-eg newPoly (1,size) -/

/- Can/should we unify this with Types.Buffer? -/
def InterpreterBuffer := Array Polynomial /- Rust: Vector of unspecified size-/
/- def BufferRef := InterpreterBuffer; Rust: Ref to a mutable array of polynomials, so unify with InterpreterBuffer-/

structure ExternHandler where
  name: String
  extra:String
  arg: Array UInt64
  outCount: size_t

def dummyHandler:ExternHandler := sorry
 /- needed in lieu of nullptr in constructor of interpreter
  TODO: see whether we can eliminate this-/

structure InterpreterRep where
  cycle: size_t/- := size_t_zero-/
  handler: ExternHandler /- := dummyHandler-/
  vals: Map MLIR_Value Polynomial /- MLIR_Value_dec := emptyMap-/
  bufs: Map MLIR_Value InterpreterBuffer /-MLIR_Value_dec := emptyMap-/

def setCycle [DecidableEq MLIR_Value] (I:InterpreterRep /-MLIR_Value Polynomial-/) (c:size_t)
    :InterpreterRep /-MLIR_Value Polynomial-/ := 
   /- { I with cycle := c }-/
   InterpreterRep.mk c I.handler I.vals I.bufs

def getCycle (I:InterpreterRep /-MLIR_Value MLIR_Value_dec-/): size_t := I.cycle

def setExternHandler (I:InterpreterRep) (h:ExternHandler):InterpreterRep := 
   { I with handler := h }

def getExternHandler (I:InterpreterRep ): ExternHandler := I.handler

def setVal (I:InterpreterRep) (v:MLIR_Value)(poly:Polynomial):InterpreterRep := 
   { I with vals := upd _ _ I.vals v poly } /-Rus refers to poly.begin() and poly.end() which I ssustes yields the array describing the coeffs-/

def getVal (I:InterpreterRep) (v: MLIR_Value):Option Polynomial := I.vals v

def setBuf (I:InterpreterRep) (B: MLIR_Value)(v:InterpreterBuffer):InterpreterRep  := 
   { I with bufs := upd _ _ I.bufs B v }

/- maye use monad?
def makeBuf (I:InterpreterRep) (B: MLIR_Value)
           (size:size_t)(kind:BufferKind): (InterpreterRep) * InterpreterBuffer :=
           let newPoly := mkNewPoly size_t_one size
           ({ I with bufs := upd _ _ I.bufs B newPoly }, b)

def getBuf (I:InterpreterRep)(B: MLIR_Value): Option MLIR_Value := I.bufs B-/

end interpreter