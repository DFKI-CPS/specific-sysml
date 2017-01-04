package de.dfki.cps.specific.sysml

import scala.util.parsing.input.Positional
import org.eclipse.uml2.uml.{ Element => UMLElement }

trait Element extends Positional {
  private [sysml] var uml = Option.empty[UMLElement]

  def at(position: Positional): this.type = {
    this.setPos(position.pos)
    this
  }
}

trait NamedElement extends Element {
  def name: String
}