package specific.sysml

import Types.Classifier
import org.eclipse.papyrus.sysml.portandflows.FlowDirection

import scala.concurrent.Future
import scala.concurrent.duration.Duration

object indent {
  def apply(lines: String) = lines.lines.map("  " + _).mkString("\n")
}

sealed abstract class DiagramKind(abbrev: String) {
  override def toString = abbrev
}

case class Comment(content: String) extends Element

object DiagramKind {
  case object ActivityDiagram extends DiagramKind("act")
  case object BlockDefinitionDiagram extends DiagramKind("bdd")
  case object InternalBlockDiagram extends DiagramKind("ibd")
  case object PackageDiagram extends DiagramKind("pkg")
  case object ParametricDiagram extends DiagramKind("par")
  case object RequirementDiagram extends DiagramKind("req")
  case object SequenceDiagram extends DiagramKind("seq")
  case object StateMachineDiagram extends DiagramKind("stm")
  case object UseCaseDiagram extends DiagramKind("uc")
}

sealed trait DiagramContent[T <: DiagramKind]

case class Diagram(diagramKind: DiagramKind, modelElementType: String, modelElementName: Name[NamedElement], diagramName: String, content: Seq[Element]) {
  override def toString = s"$diagramKind [$modelElementType] $modelElementName [$diagramName]\n" + indent(content.mkString("\n"))
}

case class Package(name: String, blocks: Seq[Block], constraints: Seq[UnprocessedConstraint], subpackages: Seq[Package]) {
  override def toString = s"<<package>> $name\n\n${blocks.mkString("\n\n")}"
}

trait Type

case class Block(rawName: String, compartments: Seq[BlockCompartment], comments: Seq[Comment]) extends Classifier(rawName) with DiagramContent[DiagramKind.BlockDefinitionDiagram.type] {
  override def toString = s"<<block>> $name\n${indent(compartments.mkString("\n"))}"
  def members = compartments.flatMap(_.content)
}
case class TypeAnnotation(name: Name[Classifier], multiplicity: Multiplicity) {
  override def toString = s": $name$multiplicity"
}

case class UnprocessedConstraint(content: Any) extends BlockMember

sealed abstract class BlockCompartment(val compartmentName: String, val content: Seq[BlockMember]) {
  override def toString = s"<<compartment>> $compartmentName\n${indent(content.mkString("\n"))}"
}
case class UnsupportedCompartment(name: String) extends BlockCompartment(name, Nil) {
  override def toString = s"<<compartment>> $name { unsupported! }"
}
case class PropertiesCompartment(properties: Seq[Property]) extends BlockCompartment("properties", properties)
case class ValuesCompartment(properties: Seq[Value]) extends BlockCompartment("values", properties)
case class OperationsCompartment(operations: Seq[Operation]) extends BlockCompartment("operations", operations)
case class ReferencesCompartment(references: Seq[Reference]) extends BlockCompartment("references", references)
case class PortsCompartment(ports: Seq[Port]) extends BlockCompartment("ports", ports)
case class BehaviorCompartment(stms: Seq[StateMachine]) extends BlockCompartment("owned behaviors", stms)
case class ConstraintsCompartment(rawConstraints: Seq[UnprocessedConstraint]) extends BlockCompartment("constraints", rawConstraints)

sealed trait BlockMember extends Element

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

case class Parameter(name: String, typeAnnotation: TypeAnnotation) extends Element {
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