package specific.uml

trait MultiplicityElement {
  val multiplicity: Multiplicity
}

case class Multiplicity(
  isOrdered: Boolean = false,
  isUnique: Boolean = true,
  lower: Int = 0,
  upper: Option[Int] = Some(1)
) {
  require(lower >= 0, "negative lower bound")
  require(upper.forall(_ >= 0), "negative upper bound")

  /** @see UML Spec 15-03-01 - Table 7.1 */
  def collectionType =
         ( isOrdered, isUnique) match {
    case ( false    , true    ) => specific.ocl.Types.SetType
    case ( true     , true    ) => specific.ocl.Types.OrderedSetType
    case ( false    , false   ) => specific.ocl.Types.BagType
    case ( true     , false   ) => specific.ocl.Types.SequenceType
  }

  def boundsString = (lower,upper) match {
    case (0,Some(1)) => None
    case (0,None) => Some("*")
    case (n,None) => Some(s"$n..*")
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