package specific.uml

trait TemplateableElement

trait Type

trait Classifier extends Type with Namespace with TemplateableElement {
  val isAbstract: Boolean
  val isFinalSpecification: Boolean
  val features: Set[Feature]
}

abstract class DataType(
  val ownedAttributes: Set[Property],
  val ownedOperations: Set[Operation]) extends Classifier

abstract class PrimitiveType(
  ownedAttributes: Set[Property],
  ownedOperations: Set[Operation]) extends DataType(ownedAttributes, ownedOperations)

abstract class Enumeration extends DataType(Set.empty, Set.empty)

abstract class InstanceSpecification

abstract class EnumerationLiteral(val enumeration: Enumeration, val classifier: Classifier) extends InstanceSpecification

trait Feature