package specific.sysml

import org.eclipse.uml2.uml

import scala.concurrent.{Future, Promise}
import scala.util.parsing.input.Positional

trait Element extends Positional {
  private [sysml] val elem = Promise[uml.Element]
  val representedElement: Future[uml.Element] = elem.future

  def at(position: Positional): this.type = {
    this.setPos(position.pos)
    this
  }
}

trait Namespace extends NamedElement with VirtualNamespace

trait VirtualNamespace {
  def qualifiedName: Option[Seq[String]]
  def members: Seq[NamedElement]

  members.foreach(_._parent = Some(this))
}

trait NamedElement extends Element {
  def name: Option[String]
  private [sysml] var _parent: Option[VirtualNamespace] = None
  def namespace = _parent

  def qualifiedName: Option[Seq[String]] = name.map { n =>
    namespace.map(_.qualifiedName).flatten.getOrElse(Seq.empty) :+ n
  }
}

trait PackageableElement extends NamedElement {
  def visibility: Option[VisibilityKind] = Some(VisibilityKind.Public)
}