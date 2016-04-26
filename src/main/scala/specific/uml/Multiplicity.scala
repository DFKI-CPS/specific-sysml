package specific.uml

trait MultiplicityElement {
  val multiplicity: Multiplicity
}

case class Multiplicity(
  isOrdered: Boolean = false,
  isUnique: Boolean = true,
  lower: BigInt,
  upper: UnlimitedNatural = UnlimitedNatural.Finite(1)
) {
  require(lower >= 0, "negative lower bound")

  /** @see UML Spec 15-03-01 - Table 7.1 */
  def collectionType =
         ( isOrdered, isUnique) match {
    case ( false    , true    ) => specific.ocl.Types.SetType
    case ( true     , true    ) => specific.ocl.Types.OrderedSetType
    case ( false    , false   ) => specific.ocl.Types.BagType
    case ( true     , false   ) => specific.ocl.Types.SequenceType
  }

  def boundsString = (lower,upper) match {
    case (x,UnlimitedNatural.Finite(y)) if x == 0 && y == 1 => None
    case (x,UnlimitedNatural.Infinity) if x == 0 => Some("*")
    case (n,UnlimitedNatural.Infinity) => Some(s"$n..*")
    case (n,m) if (n == m) => Some(s"$n")
    case (n,m) => Some(s"$n..$m")
  }

  def designatorString = {
    val ordered = if (isOrdered) Some("ordered") else None
    val nonunique = if (!isUnique) Some("nonunique") else None
    ordered.orElse(nonunique).map { _ =>
      (ordered ++ nonunique).mkString(", ")
    }.map(i => s"{ $i }")
  }
}