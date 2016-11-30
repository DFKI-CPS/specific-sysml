package specific.sysml.parser

import java.util.concurrent.TimeUnit

import specific.ocl.parser.{OclLexer, OclParsers, OclTokens}
import specific.sysml.{UnlimitedNatural, _}

import scala.concurrent.duration.{Duration, TimeUnit}
import specific.util._

import scala.util.parsing.input.Positional

object SysMLParsers extends OclParsers {
  override type Elem = OclLexer.Token

  case class PositionedName(name: String) extends Positional

  import specific.ocl.parser.OclTokens._
  import SysMLTokens._

  def elementType: Parser[String] =
    "package" | "block" | "state" ~ "machine" ^^^ "state machine"

  def diagramKind: Parser[DiagramKind] = named("diagram kind", {
    "bdd" ^^^ DiagramKind.BlockDefinitionDiagram
  })

  def diagramElementParsers(kind: DiagramKind): Parser[NamedElement] = kind match {
    case DiagramKind.BlockDefinitionDiagram => named("block or constraint", block | topLevelConstraint)
  }

  def topLevelConstraint: Parser[UnprocessedConstraint] =
    CONTEXT ~! ignoreIndentation((allExcept(CONTEXT,SimpleName("block"),DEDENT,SEPARATOR) | SEPARATOR ~ not(CONTEXT|"block"|DEDENT)).*) ^^ (x => UnprocessedConstraint(None,x))

  def diagram: Parser[Diagram] =
    ( diagramKind ~ enclosed(LEFT_SQUARE_BRACKET,elementType,RIGHT_SQUARE_BRACKET) ~ pathName[NamedElement] ~ enclosed(LEFT_SQUARE_BRACKET,name.+,RIGHT_SQUARE_BRACKET) ) >> {
      case knd ~ tpe ~ en ~ dn => separated(diagramElementParsers(knd)) ^^ (elems => Diagram(knd,tpe,en.parts,dn.mkString(" "),elems))
    }

  def pkg: Parser[Package] = PACKAGE ~> name ~ separated(block | constraint) ^^ {
    case n ~ bs => Package(n.name,bs collect every[Block], bs collect every[UnprocessedConstraint], Nil).at(n)
  }

  def block: Parser[Block] = "block" ~> name ~ indented(comment | compartment,"compartment") ^^ { case n~cs => Block(n.name,cs collect every [BlockCompartment], cs collect every [Comment]).at(n) }

  def comment: Parser[Comment] = acceptMatch("comment", {
    case SysmlComment(content) => Comment(content)
  })

  def compartment: Parser[BlockCompartment] = named("compartment",
    behaviorsCompartment
  | referencesCompartment
  | operationsCompartment
  | propertiesCompartment
  | valuesCompartment
  | portsCompartment
  | constraintsCompartment
  | commit(unsupportedCompartment) )

  def unsupportedCompartment: Parser[UnsupportedCompartment] =
    name.+ <~ skip ^^ (ns => UnsupportedCompartment(ns.mkString(" ")))

  def skip: Parser[Any] =
    INDENT ~ allExcept(INDENT,DEDENT).* ~ opt(skip) ~ allExcept(INDENT,DEDENT).* ~ DEDENT

  def behaviorsCompartment: Parser[BehaviorCompartment] =
    ( "owned" ~ "behaviors"
    | "classifier" ~ "bahavior") ~> indented(stateMachine, "state machine") ^^ BehaviorCompartment

  def constraintsCompartment: Parser[ConstraintsCompartment] =
    "constraints" ~> indented(constraint, "constraint") ^^ ConstraintsCompartment

  def stateMachine: Parser[StateMachine] =
    ("state" ~ "machine") ~> name ~ indented(state, "state") ^^ { case n~ss => StateMachine(n.name,ss).at(n) }

  def state: Parser[State] =
    ( ("initial".? <~ "state") ~ name ~ indented(transition, "transition") ^^ { case i~n~ts => ConcreteState(n.name,ts,i.isDefined).at(n) }
    | "choose" ~> indented(transition, "transition") ^^ Choice )

  def transition: Parser[Transition] = opt(trigger) ~ opt(guard) ~ opt(action) ~ (RIGHT_ARROW ~>  transitionTarget) ^^ { case t~g~a~s => Transition(t,g,a,s) }

  def transitionTarget: Parser[TransitionTarget] =
    ( state ^^ InlineTargetState
    | simpleName ^^ UnresolvedTargetStateName )

  def guard: Parser[UnprocessedConstraint] = LEFT_SQUARE_BRACKET ~> ((allExcept(RIGHT_SQUARE_BRACKET).* ^^ (UnprocessedConstraint(None,_))) <~ RIGHT_SQUARE_BRACKET)

  def action: Parser[UnprocessedConstraint] = SLASH ~> (allExcept(RIGHT_ARROW).* ^^ (UnprocessedConstraint(None,_)))

  def trigger: Parser[Trigger] =
  ( "after" ~> duration ^^ Trigger.Timeout
  | "receive" ~> name ~ opt(LEFT_PARENS ~> (name <~ RIGHT_PARENS)) ^^ { case p~v => Trigger.Receive(p.name,v.map(_.name)).at(p) } )

  def duration: Parser[Duration] = integer ~ timeUnit ^^ { case i ~ n => Duration(i.toLong,n).toCoarsest }

  def timeUnit: Parser[TimeUnit] =
  ( ("d" | "day" | "days") ^^^ TimeUnit.DAYS
  | ("h" | "hour" | "hours") ^^^ TimeUnit.HOURS
  | ("min" | "mins" | "minute" | "minutes") ^^^ TimeUnit.MINUTES
  | ("s" | "sec" | "secs" | "second" | "seconds") ^^^ TimeUnit.SECONDS
  | ("ms" | "milli" | "milis" | "millisecond" | "milliseconds") ^^^ TimeUnit.MILLISECONDS
  | ("µs" | "micro" | "micros" | "microsecond" | "microseconds") ^^^ TimeUnit.MICROSECONDS
  | ("ns" | "nano" | "nanos" | "nanosecond" | "nanoseconds") ^^^ TimeUnit.NANOSECONDS )

  def portsCompartment: Parser[PortsCompartment] =
    "ports" ~> indented(port, "port") ^^ PortsCompartment

  def port: Parser[Port] =
  ( flowDirection ~! name ~ typing ^^ { case dir ~ n ~ t => Port(n.name,Some(dir),t).at(n) }
  | name ~ typing ^^ { case n ~ t => Port(n.name,None,t).at(n) } )

  def flowDirection: Parser[FlowDirection] =
    ( IN ^^^ FlowDirection.In
    | "out" ^^^ FlowDirection.Out
    | "inout" ^^^ FlowDirection.InOut )

  def propertiesCompartment: Parser[PropertiesCompartment] =
    "properties" ~> indented(property, "property") ^^ PropertiesCompartment

  def property: Parser[Property] =
    name ~ typing ~ opt(constraint) ^^ { case name ~ tpe ~ c => Property(name.name,tpe,c).at(name) }

  def valuesCompartment: Parser[ValuesCompartment] =
    "values" ~> indented(value, "value") ^^ ValuesCompartment

  def value: Parser[Value] =
    name ~ typing ^^ { case name ~ tpe => Value(name.name,tpe).at(name) }

  def referencesCompartment: Parser[ReferencesCompartment] =
    "references" ~> indented(reference, "reference") ^^ ReferencesCompartment

  def reference: Parser[Reference] =
    name ~ typing ~ opt(opposite) ~ opt(constraint) ^^ {
      case name ~ tpe ~o~c => Reference(name.name,tpe,o.map(_.name),c).at(name)
    }

  def opposite: Parser[PositionedName] = LEFT_ARROW ~> name

  def operationsCompartment: Parser[OperationsCompartment] =
    "operations" ~> indented(operation, "operation") ^^ OperationsCompartment

  val defaultMultiplicity = Multiplicity(isOrdered = false, isUnique = false, 0, UnlimitedNatural.Finite(1))

  def operation: Parser[Operation] =
    name ~ parameterList ~ optionalTyping ~ constraint.* ~ opt(indented(ignoreIndentation(operationConstraint.*),"constraint")) ^^ {
      case name ~ ps ~ tpe ~ cs1 ~ cs2 => Operation(name.name,tpe,ps,cs1 ++ cs2.map(_.flatten).getOrElse(Nil)).at(name)
    }

  def ignoreIndentation[T](parser: => Parser[T]) = Parser(input => parser(new IndentationIgnorer(input)))

  def operationConstraint: Parser[UnprocessedConstraint] =
    ((PRE | POST) ~! opt(name) ~! COLON) ~> (allExcept(AT,PRE,POST,DEDENT) | AT ~ PRE).* ^^ (UnprocessedConstraint(None,_))

  def parameterList: Parser[Seq[Parameter]] =
    named("parameter list", enclosed(LEFT_PARENS, repsep(parameter,COMMA), RIGHT_PARENS))

  def parameter: Parser[Parameter] =
    name ~ typing ^^ { case name ~ tpe => Parameter(name.name,tpe).at(name) }

  /** @see UML Spec (15-03-01) 7.5.4 (p 77) */
  def multiplicity: Parser[Multiplicity] =
    ( enclosed(LEFT_SQUARE_BRACKET, multiplicityRange, RIGHT_SQUARE_BRACKET) ~ opt(
      enclosed(LEFT_BRACE, orderDesignator ~ opt(COMMA ~> uniquenessDesignator), RIGHT_BRACE) ^^ { case o ~ u => (o,u.getOrElse(false)) }
    | enclosed(LEFT_BRACE, uniquenessDesignator ~ opt(COMMA ~> orderDesignator), RIGHT_BRACE) ^^ { case u ~ o => (o.getOrElse(false),u) })
    ) ^^ {
      case (lb,ub)~Some((o,u)) => Multiplicity(o,u,lb.value,ub)
      case (lb,ub)~None => Multiplicity(isOrdered = false, isUnique = false, lb.value,ub)
    }

  def orderDesignator: Parser[Boolean] =
    ( "ordered" ^^^ true
    | "unordered" ^^^ false )

  def uniquenessDesignator: Parser[Boolean] =
    ( "unique" ^^^ true
    | "nonunique" ^^^ false )

  def multiplicityRange: Parser[(UnlimitedNatural,UnlimitedNatural)] =
    ( unlimitedNatural ~ ( ELIPSIS ~> unlimitedNatural) ^^ { case l~u => (l,u) }
    | unlimitedNatural ^^ {
      case UnlimitedNatural.Infinity =>
        (UnlimitedNatural.Finite(0),UnlimitedNatural.Infinity)
      case n => (n,n)
    })

  def unlimitedNatural: Parser[UnlimitedNatural] =
    ( integer ^^ UnlimitedNatural.Finite
    | STAR ^^^ UnlimitedNatural.Infinity )

  def optionalTyping: Parser[TypeAnnotation] = opt(typing).map(_.getOrElse(TypeAnnotation.Null))

  def typing: Parser[TypeAnnotation] =
    positioned(named("type signature", COLON ~> typeExp ~ opt(multiplicity)) ^^ {
      case nps~mult => TypeAnnotation(nps, mult.getOrElse(defaultMultiplicity))
    })

  def constraint: Parser[UnprocessedConstraint] = LEFT_BRACE ~! ( rep(elem("constraint content",_ != RIGHT_BRACE)) <~ RIGHT_BRACE ) ^^ {
    case _~cs => UnprocessedConstraint(None,cs.toString)
  }

  def name: Parser[PositionedName] = positioned(named("identifier", acceptMatch("identifier", {
    case n: OclTokens.SimpleName => PositionedName(n.chars)
    case k: OclTokens.Keyword => PositionedName(k.chars)
  })))

  def integer: Parser[BigInt] = named("integer", acceptMatch("integer", {
    case n: IntegerLiteral => n.value
  }))

  protected def separated[T](exprs: Parser[T]): Parser[Seq[T]] =
    SEPARATOR.* ~> (repsep(exprs,SEPARATOR.+) <~ SEPARATOR.*)

  protected def indented[T](exprs: Parser[T], what: String): Parser[Seq[T]] =
    INDENT ~! (separated(exprs) <~ DEDENT) ^^ (_._2) | success(Seq.empty)

  import scala.language.implicitConversions

  override protected implicit def keyName(what: String): Parser[String] = acceptMatch(what, {
    case n: OclTokens.SimpleName if n.chars == what => what
    case k: OclTokens.Keyword if k.chars == what => what
  })
}
