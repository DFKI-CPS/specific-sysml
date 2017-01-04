package de.dfki.cps.specific.ocl

/**
  * Created by martin on 22.04.16.
  */
sealed trait CollectionKind
object CollectionKind {
  case object Set extends CollectionKind
  case object Bag extends CollectionKind
  case object Sequence extends CollectionKind
  case object Collection extends CollectionKind
  case object OrderedSet extends CollectionKind
}
