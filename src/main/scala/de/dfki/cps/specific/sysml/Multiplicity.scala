package de.dfki.cps.specific.sysml

import scala.util.parsing.input.Positional

trait MultiplicityElement {
  val multiplicity: Multiplicity
}

case class Multiplicity(
  lower: BigInt,
  upper: UnlimitedNatural
) extends Positional {
  /** @see UML Spec 15-03-01 - Table 7.1 *
  def collectionType =
         ( isOrdered, isUnique) match {
    case ( false    , true    ) => de.dfki.cps.specific.ocl.Types.SetType
    case ( true     , true    ) => de.dfki.cps.specific.ocl.Types.OrderedSetType
    case ( false    , false   ) => de.dfki.cps.specific.ocl.Types.BagType
    case ( true     , false   ) => de.dfki.cps.specific.ocl.Types.SequenceType
  }*/

  def boundsString = (lower,upper) match {
    case (x,UnlimitedNatural.Infinity) if x == 0 => Some("*")
    case (n,UnlimitedNatural.Infinity) => Some(s"$n..*")
    case (n,UnlimitedNatural.Finite(m)) if (n == m) => Some(s"$n")
    case (n,m) => Some(s"$n..$m")
  }

  /*def designatorString = {
    val ordered = if (isOrdered) Some("ordered") else None
    val nonunique = if (!isUnique) Some("nonunique") else None
    ordered.orElse(nonunique).map { _ =>
      (ordered ++ nonunique).mkString(", ")
    }.map(i => s" { $i }")
  }*/

  override def toString = boundsString.map(x => s"[$x]").mkString
}