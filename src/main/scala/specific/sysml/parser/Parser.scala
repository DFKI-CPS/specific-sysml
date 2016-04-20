package specific.sysml.parser

import java.util.concurrent.TimeUnit

import specific.sysml._
import specific.sysml.parser.Lexer._

import scala.util.parsing.combinator.Parsers
import scala.concurrent.duration.{Duration, DurationInt, TimeUnit}

/**
  * Created by martin on 19.04.16.
  */
object Parser extends Parsers {
  override type Elem = Lexer.Token

  def pkg: Parser[Package] = PACKAGE ~> name ~ separated(block | constraint) ^^ { case n~bs => Package(n,bs.collect{ case b: Block => b},Nil) }

  def content: Parser[Seq[Block]] = separated(block)

  def block: Parser[Block] = "block" ~> name ~ indented(compartment,"compartment") ^^ { case n~cs => Block(n,cs) }

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
    "constraints" ~> indented(constraint, "constraint") ^^ (ConstraintsCompartment)

  def behaviorsCompartment: Parser[BehaviorCompartment] =
    ("owned" ~ "behaviors") ~> indented(stateMachine, "state machine") ^^ (BehaviorCompartment)

  def stateMachine: Parser[StateMachine] =
    ("state" ~ "machine") ~> opt(name) ~ indented(state, "state") ^^ { case n~ss => StateMachine(n,ss) }

  def state: Parser[State] =
    ( "state" ~> name ~ indented(transition, "transition") ^^ { case n~ts => ConcreteState(n,ts) }
    | "choose" ~> indented(transition, "transition") ^^ Choice )

  def transition: Parser[Transition] = opt(trigger) ~ opt(guard) ~ opt(action) ~ (RIGHT_ARROW ~>  transitionTarget) ^^ { case t~g~a~s => Transition(t,g,a,s) }

  def transitionTarget: Parser[TransitionTarget] =
    ( state ^^ (InlineTargetState)
    | name ^^ (UnresolvedTargetStateName) )

  def guard: Parser[UnprocessedConstraint] = LEFT_SQUARE_BRACKET ~> (constraint <~ RIGHT_SQUARE_BRACKET)

  def action: Parser[UnprocessedConstraint] = SLASH ~> constraint

  def trigger: Parser[Trigger] =
  ( "after" ~> duration ^^ Timeout
  | "receive" ~> name ~ opt(LEFT_PARENS ~> (name <~ RIGHT_PARENS)) ^^ { case p~v => Receive(p,v)} )

  def duration: Parser[Duration] = num ~ timeUnit ^^ { case i ~ n => Duration(i,n).toCoarsest }

  def timeUnit: Parser[TimeUnit] =
  ( ("d" | "day" | "days") ^^^ TimeUnit.DAYS
  | ("h" | "hour" | "hours") ^^^ TimeUnit.HOURS
  | ("min" | "mins" | "minute" | "minutes") ^^^ TimeUnit.MINUTES
  | ("s" | "sec" | "secs" | "second" | "seconds") ^^^ TimeUnit.SECONDS
  | ("ms" | "milli" | "milis" | "millisecond" | "milliseconds") ^^^ TimeUnit.MILLISECONDS
  | ("µs" | "micro" | "micros" | "microsecond" | "microseconds") ^^^ TimeUnit.MICROSECONDS
  | ("ns" | "nano" | "nanos" | "nanosecond" | "nanoseconds") ^^^ TimeUnit.NANOSECONDS )

  def portsCompartment: Parser[PortsCompartment] =
    "ports" ~> indented(port, "port") ^^ (PortsCompartment)

  def port: Parser[Port] =
  ( flowDirection ~! name ~ typing ^^ { case dir ~ n ~ t => Port(n,dir,t) }
  | name ~ typing ^^ { case n ~ t => Port(n,InOut,t) } )

  def flowDirection: Parser[FlowDirection] =
    ( IN ^^^ In
    | "out" ^^^ Out
    | "inout" ^^^ InOut )

  def propertiesCompartment: Parser[PropertiesCompartment] =
    "properties" ~> indented(property, "property") ^^ (PropertiesCompartment)

  def property: Parser[Property] =
    name ~ typing ~ opt(constraint) ^^ { case name ~ tpe ~ c => Property(name,tpe,c) }

  def valuesCompartment: Parser[ValuesCompartment] =
    "values" ~> indented(value, "value") ^^ (ValuesCompartment)

  def value: Parser[Value] =
    name ~ typing ^^ { case name ~ tpe => Value(name,tpe) }

  def referencesCompartment: Parser[ReferencesCompartment] =
    "references" ~> indented(reference, "reference") ^^ (ReferencesCompartment)

  def reference: Parser[Reference] =
    name ~ typing ~ opt(opposite) ~ opt(constraint) ^^ {
      case name ~ tpe ~o~c => Reference(name,tpe,o,c)
    }

  def opposite: Parser[String] = LEFT_ARROW ~> name

  def operationsCompartment: Parser[OperationsCompartment] =
    "operations" ~> indented(operation, "operation") ^^ (OperationsCompartment)

  def operation: Parser[Operation] =
    name ~ parameterList ~ named("return type", typing) ~ constraint.* ~ opt(indented(constraint,"constraint")) ^^ {
      case name ~ ps ~ tpe ~ cs1 ~ cs2 => Operation(name,tpe,ps,cs1 ++ cs2.getOrElse(Nil))
    }

  def parameterList: Parser[Seq[Parameter]] =
    named("parameter list", LEFT_PARENS ~> (repsep(parameter,COMMA) <~ RIGHT_PARENS))

  def parameter: Parser[Parameter] =
    name ~ typing ^^ { case name ~ tpe => Parameter(name,tpe) }

  /** @see UML Spec (15-03-01) 7.5.4 (p 77) */
  def multiplicity: Parser[Multiplicity] =
    LEFT_SQUARE_BRACKET ~> ((multiplicityRange <~ RIGHT_SQUARE_BRACKET) ~ opt(
      ((LEFT_BRACE ~> orderDesignator) ~ opt(COMMA ~> uniquenessDesignator)) <~ RIGHT_BRACE ^^ { case o ~ u => (o,u.getOrElse(false)) }
    | ((LEFT_BRACE ~> uniquenessDesignator) ~ opt(COMMA ~> orderDesignator)) <~ RIGHT_BRACE ^^ { case u ~ o => (o.getOrElse(false),u) }
    )) ^^ {
      case (lb,ub)~Some((o,u)) => Multiplicity(lb,ub,o,u)
      case (lb,ub)~None => Multiplicity(lb,ub)
    }

  def orderDesignator: Parser[Boolean] =
    ( "ordered" ^^^ true
    | "unordered" ^^^ false )

  def uniquenessDesignator: Parser[Boolean] =
    ( "unique" ^^^ true
    | "nonunique" ^^^ false )

  def multiplicityRange: Parser[(MultiplicityBound,MultiplicityBound)] =
    ( multiplicityBound ~ ( ELIPSIS ~> multiplicityBound) ^^ { case l~u => (l,u) }
    | multiplicityBound ^^ {
      case Many => (N(0),Many)
      case n => (n,n)
    })

  def multiplicityBound: Parser[MultiplicityBound] =
    num ^^ N | STAR ^^^ Many

  def typing: Parser[UnresolvedTypeAnnotation] =
    named("type signature", COLON ~> named("type name", rep1sep(name,DOT)) ~ opt(multiplicity)) ^^ {
      case nps~mult => UnresolvedTypeAnnotation(nps.mkString("."), mult.getOrElse(Multiplicity.default))
    }

  def constraint: Parser[UnprocessedConstraint] = LEFT_BRACE ~! ( rep(elem("constraint content",_ != RIGHT_BRACE)) <~ RIGHT_BRACE ) ^^ {
    case _~cs => UnprocessedConstraint(cs.toString)
  }

  implicit def keyName(what: String): Parser[String] = acceptMatch(what, {
    case n: Name if n.chars == what => what
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
