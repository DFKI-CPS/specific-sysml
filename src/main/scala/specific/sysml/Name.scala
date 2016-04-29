package specific.sysml

import scala.concurrent.{Future, Promise}

sealed trait Name {
  val parts: Seq[String]
}

case class SimpleName(name: String) extends Name {
  override def toString = s"?$name"
  val parts = Seq(name)
}

case class PathName(parts: Seq[String]) extends Name {
  override def toString = "?" + parts.mkString("::")
}

case class ResolvedName[T <: NamedElement](element: T) extends Name {
  val parts = element.name.toSeq
}