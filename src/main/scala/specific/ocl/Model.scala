package specific.ocl

sealed trait OclExpression

case class IfExp(condition: OclExpression, thenExp: OclExpression, elseExp: OclExpression) extends OclExpression
case class LetExp(varDecl: Seq[VariableDeclaration], subExp: OclExpression) extends OclExpression
case class TypeExp(referredType: Option[Classifier]) extends OclExpression

sealed trait LiteralExp extends OclExpression
case object NullLiteralExp extends LiteralExp
case object InvalidLiteralExp extends LiteralExp

sealed trait PrimitiveLiteralExp extends LiteralExp
sealed trait NumericLiteralExp extends PrimitiveLiteralExp
case class RealLiteralExp(realSymbol: Double) extends NumericLiteralExp
case class UnlimitedNaturalLiteralExp(unlimitedNaturalSymbol: BigInt) extends NumericLiteralExp
case class IntegerLiteralExp(integerSymbol: Integer) extends NumericLiteralExp
// MISSING: EnumLiteralExp

case class VariableDeclaration(name: String, tpe: Option[Classifier])

sealed trait Classifier

/**
  * AnyType is the metaclass of the special type OclAny, which is the type to which all other types conform. OclAny is the
  * sole instance of AnyType. This metaclass allows defining the special property of being the generalization of all other
  * Classifiers, including Classes, DataTypes, and PrimitiveTypes.
  */
case object AnyType extends Classifier

/**
  * VoidType is the metaclass of the OclVoid type that conforms to all types except the OclInvalid type. The only instance of
  * VoidType is OclVoid, which is further defined in the standard library. Furthermore OclVoid has exactly one instance called
  * null - corresponding to the UML NullLiteral literal specification - and representing the absence of value. Note that in
  * contrast with invalid, null is a valid value and as such can be owned by collections.
  */
case object VoidType extends Classifier

/**
  * InvalidType represents a type that conforms to all types except the VoidType type. The only instance of InvalidType is
  * Invalid, which is further defined in the standard library. Furthermore Invalid has exactly one runtime instance identified
  * as OclInvalid.
  */
case object InvalidType extends Classifier

sealed trait DataType extends Classifier

/** TupleType (informally known as record type or struct) combines different types into a single aggregate type. The parts of
  * a TupleType are described by its attributes, each having a name and a type. There is no restriction on the kind of types that
  * can be used as part of a tuple. In particular, a TupleType may contain other tuple types and collection types. Each attribute
  * of a TupleType represents a single feature of a TupleType. Each part is uniquely identified by its name.
  */
case class TupleType() extends DataType

/**
  * CollectionType describes a list of elements of a particular given type. CollectionType is a concrete metaclass whose
  * instances are the family of abstract Collection(T) data types. Its subclasses are SetType, OrderedSetType, SequenceType,
  * and BagType, whose instances are the concrete Set(T), OrderedSet(T), Sequence(T), and Bag(T), data types, respectively.
  * Part of every collection type is the declaration of the type of its elements (i.e., a collection type is parameterized with an
  * element type). In the metamodel, this is shown as an association from CollectionType to Classifier. Note that there is no
  * restriction on the element type of a collection type. This means in particular that a collection type may be parameterized
  * with other collection types allowing collections to be nested arbitrarily deep.
  */
sealed trait CollectionType extends DataType { val elementType: Classifier }

/**
  * OrderedSetType is a collection type that describes a set of elements where each distinct element occurs only once in the
  * set. The elements are ordered by their position in the sequence. Part of an OrderedSetType is the declaration of the type
  * of its elements.
  * @param elementType The type of the elements in a collection. All elements in a collection must conform to this type.
  */
case class OrderedSetType(elementType: Classifier) extends CollectionType

/**
  * SequenceType is a collection type that describes a list of elements where each element may occur multiple times in the
  * sequence. The elements are ordered by their position in the sequence. Part of a SequenceType is the declaration of the type
  * of its elements.
  * @param elementType The type of the elements in a collection. All elements in a collection must conform to this type.
  */
case class SequenceType(elementType: Classifier) extends CollectionType

/**
  * BagType is a collection type that describes a multiset of elements where each element may occur multiple times in the
  * bag. The elements are unordered. Part of a BagType is the declaration of the type of its elements.
  * @param elementType The type of the elements in a collection. All elements in a collection must conform to this type.
  */
case class BagType(elementType: Classifier) extends CollectionType

/**
  * SetType is a collection type that describes a set of elements where each distinct element occurs only once in the set. The
  * elements are not ordered. Part of a SetType is the declaration of the type of its elements.
  * @param elementType The type of the elements in a collection. All elements in a collection must conform to this type.
  */
case class SetType(elementType: Classifier) extends CollectionType

