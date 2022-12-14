/- Transcription of risczero-wip/blob/main/cirgen/compiler/IR/Types.td-/

/-namespace cirgen.compiler.ir.IR-/

section Types_dot_h

inductive StorageKind where | Normal | Reserve | Use
deriving DecidableEq
/- Types.h assigns unit32 values 0, 1, and 2 to these constructors -/
/-Needed?
def StorageKindID (s:StorageKind):UInt32 :=
  match s with
  | Normal => 0::UInt32
  | Reserve => 1::UInt32
  | Use => 2::UInt32
-/

def MLIRType := Unit
deriving DecidableEq /- TODO: replace with something more sensible-/

structure FieldInfo where
  name : String /- Types.h: mlir::StringAttr-/
  type : MLIRType /--/ Types.h: MLIR type-/
  storage: StorageKind := StorageKind.Normal
deriving DecidableEq 

/-- TODO: FieldInfoEq should be eliminated once the DecidableEq above succeeds-/
def FieldInfoEq (i j:FieldInfo):Bool :=
  (i.name == j.name) ∧ (i.type==j.type) ∧ (i.storage == j.storage)

def HashCode := UInt32 /-TODO: replace with correct digest type from SHA etc-/

def hash_value (i:FieldInfo): HashCode := 
  match i.storage with | _ => UInt32.ofNatCore 0 (by decide) /-TODO: fill some definition. Match statement is used since Lean requires the argument i to be used SOMEHWERE-/

end Types_dot_h

/-Types.cpp seems to be mostly about parsing-/

/- From Enums.td, which also defines I32 representations and mnemonics for the three constructors -/
inductive BufferKind where | Constant | Mutable | Global

section Types_dot_td
/-I don't think we want to model the types here as
inductive CirgenType where
 | Val
 | Constraint: ...
 | etc
 since constructions in eg Ops.lean take elements of these entities.
 Hence Val, Contraint, etc should be Lean4 types rather than Lean4 constructors.
 Maybe we need to wrap an inductive around all these types in the end to be
 able to refer to them collectively, though. Let's see.-/

/- Values come from the ExtensionField -/
structure Val where
  fieldP: uint64 
  fieldK: unsigned

structure Constraint where
  bogus: Unit /-TODO: what should go here?-/

/- TODO: maybe replace ValueType with type of Field?-/
structure Buffer (ValueType:Type) where
  element: ValueType
  size: unsigned
  kind: BufferKind

structure StructType where
  id: String /- in Types.td: StringRefParameter -/
  elements: Array FieldInfo /-TODO: what is "struct fields? -/

structure UnionType where
  id: String /--/ in Types.td: StringRefParameter -/
  elements: Array FieldInfo /-TODO what is "struct fields? -/

structure ArrayType where
  element: MLIRType
  size: unsigned /-TODO: Maybe use UInt32 here, or just Z?-/

inductive ContainerType where
| inl: StructType -> ContainerType
| inr: UnionType -> ContainerType

structure Ref (ValType:Type) where
  element:ValType

inductive MemberType where
| isStruct: StructType -> MemberType
| isUnion: UnionType -> MemberType
| isArray: ArrayType -> MemberType
| isRef: RefType -> MemberType

end Types_dot_td