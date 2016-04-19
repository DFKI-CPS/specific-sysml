package specific.sysml.parser

import specific.sysml._
import specific.sysml.parser.Lexer._
import scala.util.parsing.combinator.Parsers
import scala.concurrent.duration.DurationInt

/**
  * Created by martin on 19.04.16.
  */
object Parser extends Parsers {
  override type Elem = Lexer.Token

  def content: Parser[Seq[Block]] = separated(block)

  def block: Parser[Block] = BLOCK ~> name ~ indented(compartment,"compartment") ^^ { case n~cs => Block(n,cs) }

  def compartment: Parser[BlockCompartment] =
  ( referencesCompartment
  | operationsCompartment
  | propertiesCompartment
  | valuesCompartment
  | portsCompartment
  | behaviorsCompartment
  | constraintsCompartment
  | failure("expected compartment declaration"))

  def constraintsCompartment: Parser[ConstraintsCompartment] =
    CONSTRAINTS ~> indented(constraint, "constraint") ^^ (ConstraintsCompartment)

  def behaviorsCompartment: Parser[BehaviorCompartment] =
    OWNED_BEHAVIORS ~> indented(stateMachine, "state machine") ^^ (BehaviorCompartment)

  def stateMachine: Parser[StateMachine] =
    STATE_MACHINE ~> opt(name) ~ indented(state, "state") ^^ { case n~ss => StateMachine(n,ss) }

  def state: Parser[State] =
    ( STATE ~> name ~ indented(transition, "transition") ^^ { case n~ts => ConcreteState(n,ts) }
    | CHOOSE ~> indented(transition, "transition") ^^ Choice )

  def transition: Parser[Transition] = trigger ~ (RIGHT_ARROW ~> name) ^^ { case t~s => Transition(t,None,None,s) }

  def trigger: Parser[Trigger] =
  ( AFTER ~> (num <~ SECONDS) ^^ (s => Timeout(s.seconds))
  | RECEIVE ~> name ^^ (p => Receive(p,None)) )

  def portsCompartment: Parser[PortsCompartment] =
    PORTS ~> indented(port, "port") ^^ (PortsCompartment)

  def port: Parser[Port] =
  ( flowDirection ~! name ~ typing ^^ { case dir ~ n ~ t => Port(n,dir,t) }
  | name ~ typing ^^ { case n ~ t => Port(n,InOut,t) } )

  def flowDirection: Parser[FlowDirection] =
    ( IN ^^^ In
    | OUT ^^^ Out
    | INOUT ^^^ InOut )

  def propertiesCompartment: Parser[PropertiesCompartment] =
    VALUES ~> indented(property, "property") ^^ (PropertiesCompartment)

  def property: Parser[Property] =
    name ~ typing ^^ { case name ~ tpe => Property(name,tpe) }

  def valuesCompartment: Parser[ValuesCompartment] =
    VALUES ~> indented(value, "property") ^^ (ValuesCompartment)

  def value: Parser[Value] =
    name ~ typing ^^ { case name ~ tpe => Value(name,tpe) }

  def referencesCompartment: Parser[ReferencesCompartment] =
    REFERENCES ~> indented(reference, "reference") ^^ (ReferencesCompartment)

  def reference: Parser[Reference] = name ~ typing ~ multiplicity ~ opt(opposite) ^^ { case name ~ tpe ~ m ~o => Reference(name,tpe,m,o) }

  def opposite: Parser[String] = LEFT_ARROW ~> name

  def operationsCompartment: Parser[OperationsCompartment] =
    OPERATIONS ~> indented(operation, "operation") ^^ (OperationsCompartment)

  def operation: Parser[Operation] = name ~ parameterList ~ named("return type", typing) ~ constraint.* ~ opt(indented(constraint,"constraint")) ^^ { case name ~ ps ~ tpe ~ cs1 ~ cs2 => Operation(name,tpe,ps,cs1 ++ cs2.getOrElse(Nil)) }

  def parameterList: Parser[Seq[Parameter]] = named("parameter list", LEFT_PARENS ~> (repsep(parameter,COMMA) <~ RIGHT_PARENS))

  def parameter: Parser[Parameter] = name ~ typing ^^ { case name ~ tpe => Parameter(name,tpe) }

  def multiplicity: Parser[Multiplicity] = opt(LEFT_SQUARE_BRACKET ~> (multiplicityRange <~ RIGHT_SQUARE_BRACKET)) ^^ (_.getOrElse(Multiplicity.default))

  def multiplicityRange: Parser[Multiplicity] =
    ( multiplicityBound ~ ( ELIPSIS ~> multiplicityBound) ^^ { case l~u => Multiplicity(l,u) }
    | multiplicityBound ^^ {
      case Many => Multiplicity(N(0),Many)
      case n => Multiplicity(n,n)
    })

  def multiplicityBound: Parser[MultiplicityBound] = num ^^ N | STAR ^^^ Many

  def typing: Parser[String] = named("type signature", COLON ~> named("type name", rep1sep(name,DOT)) ^^ { _.mkString(".") })

  def constraint: Parser[UnprocessedConstraint] = acceptMatch("constraint", {
    case n: Constraint => UnprocessedConstraint(n.chars)
  })

  def name: Parser[String] = acceptMatch("identifier", {
    case n: Name => n.chars
  })

  def num: Parser[Int] = acceptMatch("identifier", {
    case n: Number => n.value
  })

  def named[T](name: String, p: Parser[T]): Parser[T] = p | Parser(i => Failure(s"expected $name but found ${i.first.toString}", i))

  def separated[T](exprs: Parser[T]): Parser[Seq[T]] = SEPARATOR.* ~> (repsep(exprs,SEPARATOR.+) <~ SEPARATOR.*)

  def indented[T](exprs: Parser[T], what: String): Parser[Seq[T]] = INDENT ~! (separated(exprs) <~ named(what,DEDENT)) ^^ (_._2) | success(Seq.empty)

  //implicit def elem(e: Lexer.Token): Parser[Any] = accept(e)
}
