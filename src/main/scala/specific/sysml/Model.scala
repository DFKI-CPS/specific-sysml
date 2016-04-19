package specific.sysml

import scala.concurrent.Future
import scala.concurrent.duration.Duration

/**
  * Created by martin on 18.04.16.
  */
class Model {

}

sealed trait Type

case class Block(name: String, compartments: Seq[BlockCompartment]) extends Type

case class UnprocessedConstraint(content: String)

sealed trait BlockCompartment
case class PropertiesCompartment(properties: Seq[Property]) extends BlockCompartment
case class ValuesCompartment(properties: Seq[Value]) extends BlockCompartment
case class OperationsCompartment(operations: Seq[Operation]) extends BlockCompartment
case class ReferencesCompartment(references: Seq[Reference]) extends BlockCompartment
case class PortsCompartment(ports: Seq[Port]) extends BlockCompartment
case class BehaviorCompartment(stms: Seq[StateMachine]) extends BlockCompartment
case class ConstraintsCompartment(rawConstraints: Seq[UnprocessedConstraint]) extends BlockCompartment

sealed trait MultiplicityBound
case object Many extends MultiplicityBound
case class N(n: Int) extends MultiplicityBound

case class Multiplicity(lower: MultiplicityBound, upper: MultiplicityBound)
object Multiplicity {
  def default = Multiplicity(N(0),N(1))
}

sealed trait FlowDirection
case object In extends FlowDirection
case object Out extends FlowDirection
case object InOut extends FlowDirection

case class Property(name: String, typeName: String)
case class Value(name: String, typeName: String)
case class Reference(name: String, typeName: String, multiplicity: Multiplicity, oppositeName: Option[String])
case class Operation(name: String, typeName: String, parameters: Seq[Parameter], constraints: Seq[UnprocessedConstraint])
case class Parameter(name: String, typeName: String)
case class Port(name: String, direction: FlowDirection, typeName: String)
case class StateMachine(name: Option[String], states: Seq[State])
sealed trait State
case class ConcreteState(name: String, transitions: Seq[Transition]) extends State
sealed trait PseudoState extends State
case class Choice(transitions: Seq[Transition]) extends PseudoState
case class Transition(trigger: Trigger, guard: Option[String], action: Option[String], targetName: String)

sealed trait Trigger
case class Timeout(after: Duration) extends Trigger
case class Receive(portName: String, boundVariableName: Option[String]) extends Trigger

sealed trait CallExpr