package specific.sysml.parser

import java.util.concurrent.TimeUnit

import specific.{ocl, uml}
import specific.ocl.CollectionKind
import specific.ocl.parser.OclParsers
import specific.sysml._
import specific.sysml.parser.SysMLLexer._
import specific.uml.Types.Classifier
import specific.uml.{PathName, SimpleName, UnlimitedNatural}

import scala.util.parsing.combinator.Parsers
import scala.concurrent.duration.{Duration, TimeUnit}

/**
  * Created by martin on 19.04.16.
  */
object SysMLParsers extends OclParsers {
  override type Elem = SysMLLexer.Token

  def pkg: Parser[Package] = PACKAGE ~> name ~ separated(block | constraint) ^^ { case n~bs => Package(n,bs.collect{ case b: Block => b},Nil) }

  def content: Parser[Seq[Block]] = separated(block)

  def block: Parser[Block] = "block" ~> name ~ indented(compartment,"compartment") ^^ { case n~cs => Block(n,cs) }

  def compartment: Parser[BlockCompartment] = named("compartment",
    behaviorsCompartment
  | namespaceCompartment
  | referencesCompartment
  | operationsCompartment
  | propertiesCompartment
  | valuesCompartment
  | portsCompartment
  | constraintsCompartment)

  def behaviorsCompartment: Parser[BehaviorCompartment] =
    ("owned" ~ "behaviors" |
      "classifier" ~ "bahavior") ~> indented(stateMachine, "state machine") ^^ (BehaviorCompartment)

  def namespaceCompartment: Parser[Nothing] =
    "namespace" ~! err("namespace compartments are currently unsupported") ^^ (_ => sys.error("unsupported compartment type"))

  def constraintsCompartment: Parser[ConstraintsCompartment] =
    "constraints" ~> indented(constraint, "constraint") ^^ (ConstraintsCompartment)

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

  val defaultMultiplicity = Multiplicity(UnlimitedNatural.Finite(0), UnlimitedNatural.Finite(1))

  def operation: Parser[Operation] =
    name ~ parameterList ~ opt(named("return type", typing)) ~ constraint.* ~ opt(indented(constraint,"constraint")) ^^ {
      case name ~ ps ~ tpe ~ cs1 ~ cs2 => Operation(name,tpe.getOrElse(TypeAnnotation(uml.ResolvedName(ocl.Types.VoidType), defaultMultiplicity)),ps,cs1 ++ cs2.getOrElse(Nil))
    }

  def parameterList: Parser[Seq[Parameter]] =
    named("parameter list", enclosed(LEFT_PARENS, repsep(parameter,COMMA), RIGHT_PARENS))

  def parameter: Parser[Parameter] =
    name ~ typing ^^ { case name ~ tpe => Parameter(name,tpe) }

  /** @see UML Spec (15-03-01) 7.5.4 (p 77) */
  def multiplicity: Parser[Multiplicity] =
    ( enclosed(LEFT_SQUARE_BRACKET, multiplicityRange, RIGHT_SQUARE_BRACKET) ~ opt(
      enclosed(LEFT_BRACE, orderDesignator ~ opt(COMMA ~> uniquenessDesignator), RIGHT_BRACE) ^^ { case o ~ u => (o,u.getOrElse(false)) }
    | enclosed(LEFT_BRACE, uniquenessDesignator ~ opt(COMMA ~> orderDesignator), RIGHT_BRACE) ^^ { case u ~ o => (o.getOrElse(false),u) })
    ) ^^ {
      case (lb,ub)~Some((o,u)) => Multiplicity(lb,ub,o,u)
      case (lb,ub)~None => Multiplicity(lb,ub)
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

  def primitiveType: Parser[uml.ResolvedName[Classifier]] =
    ( "Boolean" ^^^ uml.Types.Boolean
    | "Integer" ^^^ uml.Types.Integer
    | "Real" ^^^ uml.Types.Real
    | "String" ^^^ uml.Types.String
    | "UnlimitedNatural" ^^^ uml.Types.UnlimitedNatural) ^^ uml.ResolvedName[Classifier]

  def oclType: Parser[uml.ResolvedName[Classifier]] =
    ( "OclAny" ^^^ ocl.Types.AnyType
    | "OclInvalid" ^^^ ocl.Types.InvalidType
    | "OclMessage" ~! err("OclMessage is currently not supported") ^^^ ocl.Types.InvalidType
    | "OclVoid" ^^^ ocl.Types.VoidType)  ^^ uml.ResolvedName[Classifier]

  def collectionType: Parser[ocl.Types.CollectionType] =
    collectionTypeIdentifier ~ enclosed(LEFT_PARENS, typeExp, RIGHT_PARENS) ^^ { case k~t => ocl.Types.collection(k,t) }

  def collectionTypeIdentifier: Parser[CollectionKind] =
    ( "Set" ^^^ CollectionKind.Set
    | "Bag" ^^^ CollectionKind.Bag
    | "Sequence" ^^^ CollectionKind.Sequence
    | "Collection" ^^^ CollectionKind.Collection
    | "OrderedSet" ^^^ CollectionKind.OrderedSet )

  def typing: Parser[TypeAnnotation] =
    named("type signature", COLON ~> typeExp ~ opt(multiplicity)) ^^ {
      case nps~mult => TypeAnnotation(nps, mult.getOrElse(defaultMultiplicity))
    }

  def constraint: Parser[UnprocessedConstraint] = LEFT_BRACE ~! ( rep(elem("constraint content",_ != RIGHT_BRACE)) <~ RIGHT_BRACE ) ^^ {
    case _~cs => UnprocessedConstraint(cs.toString)
  }

  def name: Parser[String] = acceptMatch("identifier", {
    case n: SysMLLexer.SimpleName => n.chars
  })

  def integer: Parser[BigInt] = acceptMatch("identifier", {
    case n: IntegerLiteral => n.value
  })

  protected def separated[T](exprs: Parser[T]): Parser[Seq[T]] =
    SEPARATOR.* ~> (repsep(exprs,SEPARATOR.+) <~ SEPARATOR.*)

  protected def indented[T](exprs: Parser[T], what: String): Parser[Seq[T]] =
    enclosed(INDENT, separated(exprs), DEDENT) | success(Seq.empty)
}
