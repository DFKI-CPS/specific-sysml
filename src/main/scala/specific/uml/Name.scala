package specific.uml

sealed trait Name
sealed trait UnresolvedName extends Name
case class SimpleName(name: String) extends UnresolvedName {
  override def toString = s"?$name"
}
case class PathName(parts: Seq[SimpleName]) extends UnresolvedName {
  override def toString = "?" + parts.map(_.name).mkString("::")
}

case class ResolvedName[T <: NamedElement](element: T) extends Name {
  override def toString = element.name.getOrElse("<unnamed>")
}