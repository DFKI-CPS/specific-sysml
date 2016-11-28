package specific.ocl

import specific.ocl.Expressions.VariableDeclaration
import specific.sysml.{Name, Type}
import specific.sysml.Types.{Classifier, DataType}

/**
  * Created by martin on 22.04.16.
  */
object Types {
  /**
    * AnyType is the metaclass of the special type OclAny, which is the type to which all other types conform. OclAny is the
    * sole instance of AnyType. This metaclass allows defining the special property of being the generalization of all other
    * Classifiers, including Classes, DataTypes, and PrimitiveTypes.
    */
  case object AnyType extends Classifier("AnyType") {
    val members = Seq.empty
  }

  /**
    * VoidType is the metaclass of the OclVoid type that conforms to all types except the OclInvalid type. The only instance of
    * VoidType is OclVoid, which is further defined in the standard library. Furthermore OclVoid has exactly one instance called
    * null - corresponding to the UML NullLiteral literal specification - and representing the absence of value. Note that in
    * contrast with invalid, null is a valid value and as such can be owned by collections.
    */
  case object VoidType extends Classifier("VoidType"){
    val members = Seq.empty
  }

  /**
    * InvalidType represents a type that conforms to all types except the VoidType type. The only instance of InvalidType is
    * Invalid, which is further defined in the standard library. Furthermore Invalid has exactly one runtime instance identified
    * as OclInvalid.
    */
  case object InvalidType extends Classifier("InvalidType"){
    val members = Seq.empty
  }

  /** TupleType (informally known as record type or struct) combines different types into a single aggregate type. The parts of
    * a TupleType are described by its attributes, each having a name and a type. There is no restriction on the kind of types that
    * can be used as part of a tuple. In particular, a TupleType may contain other tuple types and collection types. Each attribute
    * of a TupleType represents a single feature of a TupleType. Each part is uniquely identified by its name.
    */
  case class TupleType(parts: Seq[VariableDeclaration]) extends DataType("TupleType"){
    val members = Seq.empty
  }

  /**
    * CollectionType describes a list of elements of a particular given type. CollectionType is a concrete metaclass whose
    * instances are the family of abstract Collection(T) data types. Its subclasses are SetType, OrderedSetType, SequenceType,
    * and BagType, whose instances are the concrete Set(T), OrderedSet(T), Sequence(T), and Bag(T), data types, respectively.
    * Part of every collection type is the declaration of the type of its elements (i.e., a collection type is parameterized with an
    * element type). In the metamodel, this is shown as an association from CollectionType to Classifier. Note that there is no
    * restriction on the element type of a collection type. This means in particular that a collection type may be parameterized
    * with other collection types allowing collections to be nested arbitrarily deep.
    */
  sealed class CollectionType(name: String = "Collection", val elementType: Name) extends DataType(name){
    val members = Seq.empty
  }

  /**
    * OrderedSetType is a collection type that describes a set of elements where each distinct element occurs only once in the
    * set. The elements are ordered by their position in the sequence. Part of an OrderedSetType is the declaration of the type
    * of its elements.
    *
    * @param elementType The type of the elements in a collection. All elements in a collection must conform to this type.
    */
  case class OrderedSetType(override val elementType: Name) extends CollectionType("OrderedSetType", elementType)

  /**
    * SequenceType is a collection type that describes a list of elements where each element may occur multiple times in the
    * sequence. The elements are ordered by their position in the sequence. Part of a SequenceType is the declaration of the type
    * of its elements.
    *
    * @param elementType The type of the elements in a collection. All elements in a collection must conform to this type.
    */
  case class SequenceType(override val elementType: Name) extends CollectionType("SequenceType", elementType)

  /**
    * BagType is a collection type that describes a multiset of elements where each element may occur multiple times in the
    * bag. The elements are unordered. Part of a BagType is the declaration of the type of its elements.
    *
    * @param elementType The type of the elements in a collection. All elements in a collection must conform to this type.
    */
  case class BagType(override val elementType: Name) extends CollectionType("BagType", elementType)

  /**
    * SetType is a collection type that describes a set of elements where each distinct element occurs only once in the set. The
    * elements are not ordered. Part of a SetType is the declaration of the type of its elements.
    *
    * @param elementType The type of the elements in a collection. All elements in a collection must conform to this type.
    */
  case class SetType(override val elementType: Name) extends CollectionType("SetType", elementType)

  def collection(kind: CollectionKind, elementType: Name): CollectionType = kind match {
    case CollectionKind.OrderedSet => OrderedSetType(elementType)
    case CollectionKind.Bag => BagType(elementType)
    case CollectionKind.Collection => new CollectionType("Collection", elementType)
    case CollectionKind.Sequence => SequenceType(elementType)
    case CollectionKind.Set => SetType(elementType)
  }

  /*/
    * MessageType describes ocl messages. Similar to the collection types, MessageType describes a set of types in the standard
    * library. Part of every MessageType is a reference to the declaration of the type of its operation or signal, i.e., an ocl
    * message type is parameterized with an operation or signal. In the metamodel, this is shown as an association from
    * MessageType to Operation and to Signal. MessageType is part of the abstract syntax of OCL, residing on M2 level. Its
    * instances, called OclMessage , and subtypes of OclMessage, reside on M1 level.
    *
    * @param referredSignal The Signal that is sent by the message.
    * @param referredOperation The Operation that is called by the message.
    */
  //case class MessageType(referredSignal: Signal, referredOperation: Operation) extends Classifier
}
