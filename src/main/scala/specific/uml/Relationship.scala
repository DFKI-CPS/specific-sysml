package specific.uml

trait Relationship extends Element {
  def relatedElements: Set[Element]
}

trait DirectedRelationship extends Relationship {
  def sources: Element
  def targets: Element
}