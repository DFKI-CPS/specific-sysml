package specific.sysml

sealed trait Name { val parts: Seq[String] }
sealed trait UnresolvedName extends Name
case class SimpleName(name: String) extends UnresolvedName {
  override def toString = s"?$name"
  val parts = Seq(name)
}
case class PathName(parts: Seq[String]) extends UnresolvedName {
  override def toString = "?" + parts.mkString("::")
}

case class ResolvedName[T <: NamedElement](element: T) extends Name {
  override def toString = element.name.getOrElse("<unnamed>")
  val parts = Nil
}