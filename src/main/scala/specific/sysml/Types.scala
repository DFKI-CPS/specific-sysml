package specific.sysml


trait TypedElement {
  val typing: Type
}

object Types {
  abstract class Classifier(val name: Option[String]) extends Type with NamedElement {
    def this(name: String) = this(Some(name))
  }
  abstract class DataType(name: String) extends Classifier(name)

  sealed abstract class PrimitiveType[R](name: String) extends DataType(name)

  /**
    * An instance of Integer is a value in the (infinite) set of integers
    * (...-2, -1, 0, 1, 2...).
    */
  case object Integer extends PrimitiveType[BigInt]("Integer")

  /**
    * An instance of Boolean is one of the predefined values true and false.
    */
  case object Boolean extends PrimitiveType[scala.Boolean]("Boolean")

  /**
    * An instance of String defines a sequence of characters. Character sets may
    * include non-Roman alphabets. The semantics of the string itself depends on
    * its purpose; it can be a comment, computational language expression,
    * OCL expression, etc.
    */
  case object String extends PrimitiveType[java.lang.String]("String")

  /**
    * An instance of UnlimitedNatural is a value in the (infinite) set of
    * natural numbers (0, 1, 2...) plus unlimited. The value of unlimited is
    * shown using an asterisk (‘*’). UnlimitedNatural values are typically used
    * to denote the upper bound of a range, such as a multiplicity; unlimited is
    * used whenever the range is specified to have no upper bound.
    */
  case object UnlimitedNatural extends
    PrimitiveType[UnlimitedNatural]("UnlimitedNatural")

  /**
    * An instance of Real is a value in the (infinite) set of real numbers.
    * Typically an implementation will internally represent Real numbers using a
    * floating point standard such as ISO/IEC/IEEE 60559:2011 (whose content is
    * identical to the predecessor IEEE 754 standard).
    */
  case object Real extends PrimitiveType[BigDecimal]("Real")
}