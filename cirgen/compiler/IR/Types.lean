/- Transcription of risczero-wip/blob/main/cirgen/compiler/IR/Types.td-/

/-namespace cirgen.compiler.ir.IR-/

section Types_dot_h

inductive StorageKind where | Normal | Reserve | Use
/- Types.h assigns unit32 values 0, 1, and 2 to these constructors -/

structure FieldInfo where
  name : String /- Types.h: mlir::StringAttr-/
  type : Type /--/ Types.h: MLIR type-/
  storage: StorageKind /- Types.h: initialized to StorageKind::Normal-/

end Types_dot_h

/-Types.cpp seems to be mostly about parsing-/

/- From Enums.td, which also defines I32 representations and mnemonics -/
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

structure Val where
  fieldP: uint64 
  fieldK: unsigned

structure Constraint where
  bogus: Unit

structure Buffer (ValueType:Type) where
  element: ValueType
  size: unsigned
  kind: BufferKind

structure StructType where
  id: String /- in Types.td: StringRefParameter -/
  elements: Array FieldInfo /-what is "struct fields? -/

structure UnionType where
  id: String /--/ in Types.td: StringRefParameter -/
  elements: Array FieldInfo /-what is "struct fields? -/

structure ArrayType where
  element: Type /- in Types.td: MLIR type-/
  size: unsigned

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