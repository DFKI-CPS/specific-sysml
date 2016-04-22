package specific.sysml

import specific.uml.Name
import specific.uml.Types.Classifier

import scala.concurrent.Future
import scala.concurrent.duration.Duration

object indent {
  def apply(lines: String) = lines.lines.map("  " + _).mkString("\n")
}

case class Package(name: String, blocks: Seq[Block], subpackages: Seq[Package]) {
  override def toString = s"<<package>> $name\n\n${blocks.mkString("\n\n")}"
}

sealed trait Type

case class Block(override val name: String, compartments: Seq[BlockCompartment]) extends Classifier(name) {
  override def toString = s"<<block>> $name\n${indent(compartments.mkString("\n"))}"
}
case class TypeAnnotation(name: Name, multiplicity: Multiplicity) {
  override def toString = s": ?'$name'$multiplicity"
}

case class UnprocessedConstraint(content: String) extends BlockMember

sealed abstract class BlockCompartment(val compartmentName: String, content: Seq[BlockMember]) {
  override def toString = s"<<compartment>> $compartmentName\n${indent(content.mkString("\n"))}"
}
case class PropertiesCompartment(properties: Seq[Property]) extends BlockCompartment("properties", properties)
case class ValuesCompartment(properties: Seq[Value]) extends BlockCompartment("values", properties)
case class OperationsCompartment(operations: Seq[Operation]) extends BlockCompartment("operations", operations)
case class ReferencesCompartment(references: Seq[Reference]) extends BlockCompartment("references", references)
case class PortsCompartment(ports: Seq[Port]) extends BlockCompartment("ports", ports)
case class BehaviorCompartment(stms: Seq[StateMachine]) extends BlockCompartment("owned behaviors", stms)
case class ConstraintsCompartment(rawConstraints: Seq[UnprocessedConstraint]) extends BlockCompartment("constraints", rawConstraints)

sealed trait MultiplicityBound
case object Many extends MultiplicityBound {
  override def toString = "*"
}
case class N(n: Int) extends MultiplicityBound {
  override def toString = n.toString
}

case class Multiplicity(
  lower: MultiplicityBound,
  upper: MultiplicityBound,
  ordered: Boolean = false,
  unique: Boolean = false) {
  private def orderedConstraint = if (ordered) "ordered" else "unordered"
  private def uniqueConstraint = if (unique) "unique" else "nonunique"
  override def toString = s"[$lower..$upper] {$orderedConstraint, $uniqueConstraint}"
}
object Multiplicity {
  def default = Multiplicity(N(0),N(1))
}

sealed trait FlowDirection {
  override def toString = this match {
    case In => "in"
    case Out => "out"
    case InOut => "inout"
  }
}
case object In extends FlowDirection
case object Out extends FlowDirection
case object InOut extends FlowDirection

sealed trait BlockMember

case class Property(name: String, typeAnnotation: TypeAnnotation, constraint: Option[UnprocessedConstraint]) extends BlockMember

case class Value(name: String, typeAnnotation: TypeAnnotation) extends BlockMember {
  override def toString = s"<<value>> $name$typeAnnotation"
}

case class Reference(
    name: String,
    typeAnnotation: TypeAnnotation,
    oppositeName: Option[String],
    constraint: Option[UnprocessedConstraint]) extends BlockMember {
  override def toString = s"<<reference>> $name$typeAnnotation" + oppositeName.map(x =>s" <- $x").getOrElse("")
}
case class Operation(
    name: String,
    typeAnnotation: TypeAnnotation,
    parameters: Seq[Parameter],
    constraints: Seq[UnprocessedConstraint]) extends BlockMember {
  override def toString = s"<<operation>> $name(${parameters.mkString})$typeAnnotation"
}

case class Parameter(name: String, typeAnnotation: TypeAnnotation) {
  override def toString = s"<<param>> $name$typeAnnotation"
}

case class Port(name: String, direction: FlowDirection, typeAnnotation: TypeAnnotation) extends BlockMember {
  override def toString = s"<<port>> $direction $name$typeAnnotation"
}

case class StateMachine(name: Option[String], states: Seq[State]) extends BlockMember {
  override def toString = s"<<state machine>> ${name.getOrElse("<unnamed>")}\n${indent(states.mkString("\n"))}"
}

sealed trait State

case class ConcreteState(name: String, transitions: Seq[Transition]) extends State {
  override def toString = s"<<state>> $name\n${indent(transitions.mkString("\n"))}"
}

sealed trait PseudoState extends State

case class Choice(transitions: Seq[Transition]) extends PseudoState {
  override def toString = s"<<choice pseudo state>>\n${indent(transitions.mkString("\n"))}"
}

case class Transition(
    trigger: Option[Trigger],
    guard: Option[UnprocessedConstraint],
    action: Option[UnprocessedConstraint],
    target: TransitionTarget) {
  override def toString = s"<<transition>> ${trigger.map(_.toString).getOrElse("")} [${guard.map(_.toString).getOrElse("")}] / ${action.map(_.toString).getOrElse("")} -> $target"
}

sealed trait TransitionTarget

case class InlineTargetState(state: State) extends TransitionTarget {
  override def toString = state.toString
}

case class UnresolvedTargetStateName(name: String) extends TransitionTarget {
  override def toString = s"?'$name'"
}

sealed trait Trigger
case class Timeout(after: Duration) extends Trigger
case class Receive(portName: String, boundVariableName: Option[String]) extends Trigger

sealed trait CallExpr