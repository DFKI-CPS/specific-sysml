package specific.uml

trait Namespace extends NamedElement {
  def members: Set[Element]
}

trait NamedElement extends Element {
  def name: Option[String]
  def qualifiedName: Option[String] = ???
  def visibility: Option[VisibilityKind] = ???
  def ownedMembers: Set[NamedElement] = ???
  def member: Set[NamedElement] = ???
}

trait PackageableElement extends NamedElement {
  override def visibility: Option[VisibilityKind] = Some(VisibilityKind.Public)
}