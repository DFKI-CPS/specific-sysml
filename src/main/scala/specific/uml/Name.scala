package specific.uml

trait NamedElement {
  val name: String
}

sealed trait Name
sealed trait UnresolvedName extends Name
case class SimpleName(name: String) extends UnresolvedName
case class PathName(parts: Seq[SimpleName]) extends UnresolvedName

case class ResolvedName[T <: NamedElement](element: T) extends Name