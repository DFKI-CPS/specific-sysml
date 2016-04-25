package specific.uml

trait Namespace extends NamedElement {

}

trait NamedElement extends Element {
  def name: Option[String]
  def qualifiedName: Option[String]
  def visibility: Option[VisibilityKind]
  def ownedMembers: Set[NamedElement]
  def member: Set[NamedElement]
}

trait PackageableElement extends NamedElement {
  def visibility: Option[VisibilityKind] = Some(VisibilityKind.Public)
}