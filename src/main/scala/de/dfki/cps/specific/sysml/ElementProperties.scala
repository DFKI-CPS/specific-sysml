package de.dfki.cps.specific.sysml

import scala.util.parsing.input.Positional

sealed trait TypedElementProperty extends Positional
sealed trait OperationProperty extends TypedElementProperty
sealed trait AttributeProperty extends TypedElementProperty
sealed trait ReferenceProperty extends TypedElementProperty

object TypedElementProperty {
  case class Ordered(value: Boolean) extends TypedElementProperty with AttributeProperty with OperationProperty with ReferenceProperty
  case class Unique(value: Boolean) extends TypedElementProperty with AttributeProperty with OperationProperty with ReferenceProperty
}

object OperationProperty {
  case class Query(value: Boolean) extends OperationProperty
}

object ReferenceProperty {
  case class Subsets(ref: Name) extends ReferenceProperty
}

object AttributeProperty {
  case class ReadOnly(value: Boolean) extends AttributeProperty
}