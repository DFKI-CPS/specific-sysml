package de.dfki.cps.specific.sysml

trait Namespace extends NamedElement with VirtualNamespace

trait VirtualNamespace {
  def members: Seq[NamedElement]
}

trait PackageableElement extends NamedElement {
  def visibility: Option[VisibilityKind] = Some(VisibilityKind.Public)
}