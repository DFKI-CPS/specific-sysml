package de.dfki.cps.specific.sysml

import de.dfki.cps.specific.sysml.Types.Classifier
import scala.concurrent.duration.Duration
import scala.util.parsing.input.{Positional}




sealed abstract class DiagramKind(abbrev: String) {
  override def toString: String = abbrev
}

case class Comment(content: String) extends Element

case class Mapping(supplier: String, client: UnprocessedConstraint, subMappings: Seq[Mapping]) extends Element

case class Satisfy(requirement: Name, elements: Seq[Name]) extends Element

case class Project(name: String, includes: Seq[String], mappings: Seq[Mapping], satisfy: Seq[Satisfy]) extends NamedElement

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
  val name: String = diagramName
  override def toString: String = s"$diagramKind [$modelElementType] $modelElementName [$diagramName]\n" + indent(members.mkString("\n"))
}

case class Package(name: String, blocks: Seq[Block], constraints: Seq[UnprocessedConstraint], subpackages: Seq[Package]) extends Namespace {
  override def toString = s"<<package>> $name\n\n${blocks.mkString("\n\n")}"
  def members: Seq[NamedElement] = blocks ++ constraints
}

case class Realization(name: String, supplier: Name, client: UnprocessedConstraint) extends NamedElement

trait Type

case class Block(rawName: String, compartments: Seq[BlockCompartment], comments: Seq[Comment]) extends Classifier(rawName) with DiagramContent[DiagramKind.BlockDefinitionDiagram.type] {
  override def toString = s"<<block>> $name\n${indent(compartments.mkString("\n"))}"
  def members: Seq[BlockMember] = compartments.flatMap(_.content)
}

case class TypeAnnotation(name: Name, multiplicity: Option[Multiplicity]) extends Positional {
  override def toString = s": $name$multiplicity"
  def isMany = multiplicity.exists(x => x.upper.isInfinity || x.upper.value > 1)
}

object TypeAnnotation{
  val Unit = TypeAnnotation(ResolvedName(Types.Unit), None)
  val Null = TypeAnnotation(ResolvedName(Types.Null), None)
}

sealed trait ConstraintType
object ConstraintType {
  case object Inv extends ConstraintType
  case object Post extends ConstraintType
  case object Pre extends ConstraintType
  case object Body extends ConstraintType
  case object Init extends ConstraintType
  case object Derive extends ConstraintType
  case object Query extends ConstraintType
}

case class UnprocessedConstraint(tpe: ConstraintType, constraintName: Option[SimpleName], content: String) extends BlockMember {
  def name = "[[constraint]]"
}

sealed abstract class BlockCompartment(val compartmentName: String, val content: Seq[BlockMember]) extends Element {
  override def toString = s"<<compartment>> $compartmentName\n${indent(content.mkString("\n"))}"
}
case class UnsupportedCompartment(name: String) extends BlockCompartment(name, Nil) {
  override def toString = s"<<compartment>> $name { unsupported! }"
}
case class PropertiesCompartment(properties: Seq[Property]) extends BlockCompartment("properties", properties)
case class ValuesCompartment(properties: Seq[Property]) extends BlockCompartment("values", properties)
case class OperationsCompartment(operations: Seq[Operation]) extends BlockCompartment("operations", operations)
case class ReferencesCompartment(references: Seq[Reference]) extends BlockCompartment("references", references)
case class PortsCompartment(ports: Seq[Port]) extends BlockCompartment("ports", ports)
case class BehaviorCompartment(stms: Seq[StateMachine]) extends BlockCompartment("owned behaviors", stms)
case class ConstraintsCompartment(rawConstraints: Seq[UnprocessedConstraint]) extends BlockCompartment("constraints", rawConstraints)

sealed trait BlockMember extends NamedElement

case class Property(
  name: String,
  typeAnnotation: TypeAnnotation,
  properties: Seq[AttributeProperty],
  constraints: Seq[UnprocessedConstraint]) extends BlockMember

case class Reference(
    name: String,
    typeAnnotation: TypeAnnotation,
    isComposite: Boolean,
    oppositeName: Option[String],
    properties: Seq[ReferenceProperty],
    constraints: Seq[UnprocessedConstraint]) extends BlockMember with TypedElement {
  override def toString: String = s"<<reference>> $name$typeAnnotation" + oppositeName.map(x =>s" <- $x").getOrElse("")
  def isMany = typeAnnotation.isMany
}

case class Operation(
    name: String,
    typeAnnotation: TypeAnnotation,
    parameters: Seq[Parameter],
    properties: Seq[OperationProperty],
    constraints: Seq[UnprocessedConstraint]) extends BlockMember {
  override def toString: String = s"<<operation>> $name(${parameters.mkString})$typeAnnotation"
}

case class Parameter(
  name: String,
  typeAnnotation: TypeAnnotation,
  properties: Seq[TypedElementProperty]) extends Element {
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

case class StateMachine(name: String, states: Seq[State], operation: Option[String]) extends BlockMember with Namespace {
  override def toString = s"<<state machine>> $name\n${indent(states.mkString("\n"))}"
  def members: Seq[State] = states
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
  override def toString: String = state.toString
}

case class UnresolvedTargetStateName(name: Name) extends TransitionTarget {
  override def toString: String = name.toString
}

case class TimeEvent(duration: Duration) extends Positional

sealed trait Trigger extends Element
object Trigger {
  case class Timeout(after: TimeEvent) extends Trigger
  case class Receive(portName: String, boundVariableName: Option[String]) extends Trigger
  case class Call(operation: String) extends Trigger
}

sealed trait CallExpr

sealed trait ShortConstraint
object ShortConstraint {
  case class Subsets(expr: String) extends ShortConstraint
}

case class Requirement(name: String, text: String) extends NamedElement with DiagramContent[DiagramKind.RequirementDiagram.type]