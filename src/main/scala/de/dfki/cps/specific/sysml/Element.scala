package de.dfki.cps.specific.sysml

import scala.util.parsing.input.Positional

trait FilePositional extends Positional {
  var file: String = "nosource://"
}

trait Element extends FilePositional {

  def at(position: FilePositional): this.type = {
    this.setPos(position.pos)
    this.file = position.file
    this
  }
}

trait NamedElement extends Element {
  def name: String
}