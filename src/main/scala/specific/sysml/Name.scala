package specific.sysml

import scala.concurrent.{Future, Promise}

sealed trait Name[T <: NamedElement] {
  private [sysml] val elem = Promise[T]
  private [sysml] def -->(t: T) = elem.success(t)
  val element: Future[T] = elem.future
  val parts: Seq[String]
}

case class SimpleName[T <: NamedElement](name: String) extends Name[T] {
  override def toString = s"?$name"
  val parts = Seq(name)
}

case class PathName[T <: NamedElement](parts: Seq[String]) extends Name[T] {
  override def toString = "?" + parts.mkString("::")
}

case class ResolvedName[T <: NamedElement](el: T) extends Name[T] {
  val parts = el.name.toSeq
  override val element = Future.successful(el)
}