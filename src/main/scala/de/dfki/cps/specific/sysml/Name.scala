package de.dfki.cps.specific.sysml

import scala.util.parsing.input.Positional

sealed trait Name extends Positional {
  val parts: Seq[String]
  def at(pos: Positional): this.type = {
    this.pos = pos.pos
    this
  }
}

case class SimpleName(name: String) extends Name {
  override def toString = name
  val parts = Seq(name)
}

case class PathName(parts: Seq[String]) extends Name {
  override def toString = parts.mkString("::")
}

case class ResolvedName[T <: NamedElement](element: T) extends Name {
  val parts = Seq(element.name)
  override def toString = element.name
}