package specific.sysml

import scala.concurrent.{ExecutionContext, Future}

trait Namespace extends NamedElement with VirtualNamespace

trait VirtualNamespace {
  def members: Seq[NamedElement]
}

trait PackageableElement extends NamedElement {
  def visibility: Option[VisibilityKind] = Some(VisibilityKind.Public)
}