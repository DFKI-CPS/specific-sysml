package specific.uml

trait Comment extends Element {
  def body: Option[String]
  def owningElement: Option[Element]
  def annotatedElements: Set[Element]
}
