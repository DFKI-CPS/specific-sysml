package de.dfki.cps.specific.sysml.mappings

import org.eclipse.uml2.uml.{NamedElement, Transition}

sealed abstract trait Mapping[T <: NamedElement] {
  val source: T
}

case class DataMapping[T <: NamedElement](
  source: T,
  expr: String) extends Mapping[T]

case class BehaviorMapping(
  source: Transition,
  ins: Set[Transition],
  outs: Set[Transition]) extends Mapping[Transition]