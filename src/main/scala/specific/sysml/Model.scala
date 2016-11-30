package specific.sysml

import Types.Classifier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.parsing.input.{Position, Positional}


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

case class Diagram(diagramKind: DiagramKind, modelElementType: String, modelElementName: Seq[String], diagramName: String, members: Seq[NamedElement]) extends Namespace {
  val name = diagramName
  override def toString = s"$diagramKind [$modelElementType] $modelElementName [$diagramName]\n" + indent(members.mkString("\n"))
}

case class Package(name: String, blocks: Seq[Block], constraints: Seq[UnprocessedConstraint], subpackages: Seq[Package]) extends Namespace {
  override def toString = s"<<package>> $name\n\n${blocks.mkString("\n\n")}"
  def members = blocks ++ constraints
}

trait Type

case class Block(rawName: String, compartments: Seq[BlockCompartment], comments: Seq[Comment]) extends Classifier(rawName) with DiagramContent[DiagramKind.BlockDefinitionDiagram.type] {
  override def toString = s"<<block>> $name\n${indent(compartments.mkString("\n"))}"
  def members = compartments.flatMap(_.content)
}

case class TypeAnnotation(name: Name, multiplicity: Multiplicity) extends Positional {
  override def toString = s": $name$multiplicity"
}

object TypeAnnotation{
  val Null = TypeAnnotation(ResolvedName(Types.Null), Multiplicity(false,false,0,UnlimitedNatural.Finite(0)))
}

case class UnprocessedConstraint(content: String) extends BlockMember {
  def name = "<<anonymous>>"
}

sealed abstract class BlockCompartment(val compartmentName: String, val content: Seq[BlockMember]) extends Element {
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

sealed trait BlockMember extends NamedElement

case class Property(
  name: String,
  typeAnnotation: TypeAnnotation,
  constraint: Option[UnprocessedConstraint]) extends BlockMember

case class Value(name: String, typeAnnotation: TypeAnnotation) extends BlockMember {
  override def toString = s"<<value>> $name$typeAnnotation"
}

case class Reference(
    name: String,
    typeAnnotation: TypeAnnotation,
    oppositeName: Option[String],
    constraint: Option[UnprocessedConstraint]) extends BlockMember with TypedElement {
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

sealed trait FlowDirection
object FlowDirection {
  case object In extends FlowDirection
  case object Out extends FlowDirection
  case object InOut extends FlowDirection
}

case class Port(name: String, direction: Option[FlowDirection], typeAnnotation: TypeAnnotation) extends BlockMember {
  override def toString = s"<<port>> $direction $name$typeAnnotation"
}

case class StateMachine(name: String, states: Seq[State]) extends BlockMember with Namespace {
  override def toString = s"<<state machine>> $name\n${indent(states.mkString("\n"))}"
  def members = states
}

sealed trait State extends NamedElement

case class ConcreteState(name: String, transitions: Seq[Transition], isInitial: Boolean) extends State {
  override def toString = s"<<state>> $name\n${indent(transitions.mkString("\n"))}"
}

sealed trait PseudoState extends State

case class Choice(transitions: Seq[Transition]) extends PseudoState {
  val name = "<<anonymous>>"
  override def toString = s"<<choice pseudo state>>\n${indent(transitions.mkString("\n"))}"
}

case class Transition(
    trigger: Option[Trigger],
    guard: Option[UnprocessedConstraint],
    action: Option[UnprocessedConstraint],
    target: TransitionTarget) extends Element {
  override def toString = s"<<transition>> ${trigger.map(_.toString).getOrElse("")} [${guard.map(_.toString).getOrElse("")}] / ${action.map(_.toString).getOrElse("")} -> $target"
}

sealed trait TransitionTarget extends Element

case class InlineTargetState(state: State) extends TransitionTarget {
  override def toString = state.toString
}

case class UnresolvedTargetStateName(name: Name) extends TransitionTarget {
  override def toString = name.toString
}

sealed trait Trigger extends Element
object Trigger {
  case class Timeout(after: Duration) extends Trigger
  case class Receive(portName: String, boundVariableName: Option[String]) extends Trigger
  //case class Call(operation)
}

sealed trait CallExpr