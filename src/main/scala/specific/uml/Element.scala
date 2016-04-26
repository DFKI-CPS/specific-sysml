package specific.uml

import specific.util._

trait Element {
  def comments: Set[Comment] = ???
  def ownedElements: Set[Element] = ???

  def ownedComments: Set[Comment] =
    ownedElements collect every [Comment]

  def relationships: Set[Relationship] =
    ownedElements collect every [Relationship]

  def directedRelationships: Set[DirectedRelationship] =
    relationships collect every [DirectedRelationship]
}