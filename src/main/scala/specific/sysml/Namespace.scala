package specific.sysml

import org.eclipse.uml2.uml

import scala.concurrent.{Future, Promise}

trait Path {
  val path: Seq[String]
}

trait Element {
  private [sysml] val elem = Promise[uml.Element]
  val representedElement: Future[uml.Element] = elem.future
}

trait Namespace extends NamedElement with Path

trait NamedElement extends Element {
  def name: Option[String]
}

trait PackageableElement extends NamedElement {
  def visibility: Option[VisibilityKind] = Some(VisibilityKind.Public)
}