package specific.uml

trait Element {
  def comments: Set[Comment]
  def ownedElements: Set[Element]

  def ownedComments: Set[Comment] =
    ownedElements.collect { case c: Comment => c }

  def relationships: Set[Relationship] =
    ownedElements.collect { case r: Relationship => r }

  def directedRelationships: Set[DirectedRelationship] =
    relationships.collect { case d: DirectedRelationship => d }
}