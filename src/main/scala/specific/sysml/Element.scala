package specific.sysml

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.parsing.input.Positional

trait Element extends Positional {
  private [sysml] var uml = Option.empty[org.eclipse.uml2.uml.Element]

  def at(position: Positional): this.type = {
    this.setPos(position.pos)
    this
  }
}

trait NamedElement extends Element {
  def name: String
  private [sysml] var _parent = Promise[VirtualNamespace]
  def namespace = _parent.future
}