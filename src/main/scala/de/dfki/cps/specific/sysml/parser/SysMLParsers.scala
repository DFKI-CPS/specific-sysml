package specific.sysml.parser

import java.util.concurrent.TimeUnit

import de.dfki.cps.specific.ocl.parser.{OclLexer, OclParsers, OclTokens}
import de.dfki.cps.specific.sysml
import de.dfki.cps.specific.sysml._

import scala.concurrent.duration.{Duration, TimeUnit}
import de.dfki.cps.specific.util._

import scala.util.parsing.input.Positional

object SysMLParsers extends OclParsers {
  override type Elem = OclLexer.Token

  case class PositionedName(name: String) extends FilePositional

  import de.dfki.cps.specific.ocl.parser.OclTokens._
  import SysMLTokens._

  def elementType: Parser[String] =
    "model" | "package" | "block" | "state" ~ "machine" ^^^ "state machine"

  def diagramKind: Parser[DiagramKind] = named("diagram kind", {
    "bdd" ^^^ DiagramKind.BlockDefinitionDiagram |
    "pkg" ^^^ DiagramKind.PackageDiagram |
    "req" ^^^ DiagramKind.RequirementDiagram
  })

  def diagramElementParsers(kind: DiagramKind): Parser[NamedElement] = kind match {
    case DiagramKind.BlockDefinitionDiagram => named("block or constraint", block)
    case DiagramKind.RequirementDiagram => named("requirement", requirement)
    case other => failure(s"unsupported diagram type $other")
  }

  def requirement: Parser[NamedElement] =
    "requirement" ~> name ~ indented(textLine, "requirement text") ^^ {
      case n~ls => Requirement(n.name,ls.mkString(" ").replaceAll("\\s+"," ")) at n
    }

  def textLine: Parser[String] = Parser[String] { input =>
    val first = input.offset
    val parser = allExcept(INDENT,DEDENT,SEPARATOR).+
    parser(input) match {
      case Success(_,next) =>
        val c = input.source.subSequence(first, next.offset).toString
        Success(c,next)
      case err: NoSuccess => err
    }
  }

  def realization: Parser[Mapping] = (name <~ (REALIZATION ~ MINUS.*)) ~ captureConstraint(ConstraintType.Query,None,allExcept(INDENT,SEPARATOR,DEDENT,EOF).*) ~ opt(indented(realization,"sub realization")) ^^ {
    case n~c~ss => Mapping(n.name,c,ss.getOrElse(Nil)) at n
  }

  def traceElements: Parser[Seq[Name]] = (
    LEFT_ARROW ~> pathName[NamedElement] ^^ { case x => Seq(x) }
  | indented(LEFT_ARROW ~> pathName, "satisfying element")
  )

  def satisfy: Parser[Satisfy] = ("satisfy" ~! pathName[NamedElement] ~ traceElements) ^^ {
    case _ ~ n ~ elems => Satisfy(n,elems)
  }

  def include: Parser[Seq[String]] =
    ( "include" ~> textLine ^^ (l => Seq(l))
    | "include" ~> indented(textLine, "file location") )


  def file = diagram | project

  def diagram: Parser[Diagram] =
    ( diagramKind ~! enclosed(LEFT_SQUARE_BRACKET,elementType,RIGHT_SQUARE_BRACKET) ~ pathName[NamedElement] ~ enclosed(LEFT_SQUARE_BRACKET,name.+,RIGHT_SQUARE_BRACKET) ) >> {
      case knd ~ tpe ~ en ~ dn => separated(diagramElementParsers(knd)) ^^ (elems => Diagram(knd,tpe,en.parts,dn.mkString(" "),elems))
    }

  def project: Parser[Project] =
    "project" ~! enclosed(LEFT_SQUARE_BRACKET,name.+,RIGHT_SQUARE_BRACKET) ~
       separated(include | realization | satisfy) ^^ {
      case _ ~ name ~ elems => Project(name.mkString(" "),(elems collect every[Seq[String]]).flatten,elems collect every[Mapping],elems collect every[Satisfy])
    }


  def pkg: Parser[Package] = PACKAGE ~> name ~ separated(block) ^^ {
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
    ( "owned".? ~ "behaviors"
    | "classifier" ~ "bahavior") ~> indented(stateMachine, "state machine") ^^ BehaviorCompartment

  def constraintsCompartment: Parser[ConstraintsCompartment] =
    "constraints" ~> indented(constraint(ConstraintType.Inv), "constraint") ^^ ConstraintsCompartment

  def stateMachine: Parser[StateMachine] =
    ("state" ~ "machine") ~> name ~ opt(REALIZATION ~> name) ~ indented(state, "state") ^^ { case n~r~ss => StateMachine(n.name,ss,r.map(_.name)).at(n) }

  def state: Parser[State] =
    ( ("initial".? <~ "state") ~ name ~ indented(transition, "transition") ^^ { case i~n~ts => ConcreteState(n.name,ts,i.isDefined).at(n) }
    | positioned("choose" ~> indented(transition, "transition") ^^ Choice) )

  def transition: Parser[Transition] = positioned(opt(trigger) ~ opt(guard) ~ (SLASH ~> opt(action)) ~ (RIGHT_ARROW ~>  transitionTarget) ^^ { case t~g~a~s => Transition(t,g,a,s) })

  //def shortConstraint

  def transitionTarget: Parser[TransitionTarget] =
    ( state ^^ InlineTargetState
    | simpleName ^^ UnresolvedTargetStateName )

  def guard: Parser[UnprocessedConstraint] =
    LEFT_SQUARE_BRACKET ~> constraintContent(ConstraintType.Query, None, RIGHT_SQUARE_BRACKET) <~ RIGHT_SQUARE_BRACKET

  def action: Parser[UnprocessedConstraint] =
    constraintContent(ConstraintType.Query, None, RIGHT_ARROW)

  def trigger: Parser[Trigger] =
  positioned(
    "after" ~> duration ^^ Trigger.Timeout
  | "receive" ~> name ~ opt(LEFT_PARENS ~> (name <~ RIGHT_PARENS)) ^^ { case p~v => Trigger.Receive(p.name,v.map(_.name)).at(p) }
  | name ^^ { case x => Trigger.Call(x.name) at x } )

  def duration: Parser[TimeEvent] = positioned{
    integer ~ timeUnit ^^ { case i ~ n => TimeEvent(Duration(i.toLong,n).toCoarsest) }
  }

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
  positioned( flowDirection ~! name ~ typing ^^ { case dir ~ n ~ t => Port(n.name,Some(dir),t).at(n) }
  | name ~ typing ^^ { case n ~ t => Port(n.name,None,t).at(n) } )

  def flowDirection: Parser[FlowDirection] =
    ( IN ^^^ FlowDirection.In
    | "out" ^^^ FlowDirection.Out
    | "inout" ^^^ FlowDirection.InOut )

  def propertiesCompartment: Parser[PropertiesCompartment] =
    "properties" ~> indented(property, "property") ^^ PropertiesCompartment

  def property: Parser[Property] =
    name ~ typing ~ opt(propertyList[AttributeProperty](uniqueProperty,orderedProperty)) ~ opt(indented(ignoreIndentation(propertyConstraint).*,"constraint")) ^^ {
      case name ~ tpe ~ ps ~ cs => Property(name.name,tpe,ps.toSeq.flatten,cs.map(_.flatten).getOrElse(Nil)).at(name)
    }

  def propertyConstraint: Parser[UnprocessedConstraint] =
    (((DERIVE | INIT) ^^ tokenToConstraintType) ~ opt(simpleName) <~ COLON ) >> { case tpe~n =>
      captureConstraint(tpe,n,(opt(name ~ COLON)) ~> (allExcept(AT,PRE,POST,DEDENT) | AT ~ PRE).*)}

  def valuesCompartment: Parser[ValuesCompartment] =
    "values" ~> indented(property, "value") ^^ ValuesCompartment

  def referencesCompartment: Parser[ReferencesCompartment] =
    "references" ~> indented(reference, "reference") ^^ ReferencesCompartment

  def reference: Parser[Reference] =
    name ~ typing ~ opt(propertyList[ReferenceProperty](orderedProperty,uniqueProperty,subsetsProperty)) ~ opt(opposite) ~ opt(indented(ignoreIndentation(propertyConstraint).*,"constraint")) ^^ {
      case name ~ tpe ~ ps ~ opp ~ cs => Reference(name.name,tpe,opp.map(_._1).getOrElse(false),opp.flatMap(_._2.map(_.name)),ps.toSeq.flatten,cs.map(_.flatten).getOrElse(Nil)).at(name)
    }

  def opposite: Parser[(Boolean,Option[PositionedName])] = ((LEFT_ARROW ^^^ false | NOT_EQUAL ^^^ true) ~ opt(name)) ^^ { case a~b => (a,b) }

  def operationsCompartment: Parser[OperationsCompartment] =
    "operations" ~> indented(operation, "operation") ^^ OperationsCompartment

  def operation: Parser[Operation] =
    name ~ parameterList ~ opt(typing) ~ opt(propertyList[OperationProperty](orderedProperty,uniqueProperty,queryProperty)) ~ opt(indented(ignoreIndentation(operationConstraint).*,"constraint")) ^^ {
      case name ~ ps ~ tpe ~ propss ~ cs => Operation(name.name,tpe.getOrElse(TypeAnnotation.Unit),ps,propss.toSeq.flatten,cs.map(_.flatten).getOrElse(Nil)).at(name)
    }

  def ignoreIndentation[T](parser: => Parser[T]) = Parser(input => parser(new IndentationIgnorer(input)))

  private def captureConstraint(tpe: ConstraintType, name: Option[sysml.SimpleName], parser: Parser[Any]) = Parser[UnprocessedConstraint] { input =>
    val start = input.pos
    val first = input.offset
    parser(input) match {
      case Success(_,next) =>
        val c = UnprocessedConstraint(tpe, name, input.source.subSequence(first + 1, next.offset).toString)
        c.pos = start
        Success(c,next)
      case err: NoSuccess => err
    }
  }

  def tokenToConstraintType(elem: Elem): ConstraintType = elem match {
    case PRE => ConstraintType.Pre
    case POST => ConstraintType.Post
    case BODY => ConstraintType.Body
    case INV => ConstraintType.Inv
    case DERIVE => ConstraintType.Derive
    case INIT => ConstraintType.Init
  }

  def pname: Parser[sysml.SimpleName] =
    name ^^ (n => sysml.SimpleName(n.name) at n)

  def operationConstraint: Parser[UnprocessedConstraint] =
    (((PRE | POST | BODY) ^^ tokenToConstraintType) ~ opt(pname) <~ COLON ) >> { case tpe ~ n =>
      captureConstraint(tpe, n, (allExcept(AT, PRE, POST, DEDENT) | AT ~ PRE).*)
    }

  def parameterList: Parser[Seq[Parameter]] =
    named("parameter list", enclosed(LEFT_PARENS, repsep(parameter,COMMA), RIGHT_PARENS))

  def parameter: Parser[Parameter] =
    name ~ typing ~ opt(propertyList[TypedElementProperty](orderedProperty,uniqueProperty)) ^^ { case name ~ tpe ~ ps => Parameter(name.name,tpe,ps.toSeq.flatten).at(name) }

  /** @see UML Spec (15-03-01) 7.5.4 (p 77) */
  def multiplicity: Parser[Multiplicity] =
    enclosed(LEFT_SQUARE_BRACKET, multiplicityRange, RIGHT_SQUARE_BRACKET)

  def multiplicityRange: Parser[Multiplicity] =
    positioned( integer ~ ( ELIPSIS ~> unlimitedNatural) ^^ { case l~u => Multiplicity(l,u) }
    | unlimitedNatural ^^ {
      case UnlimitedNatural.Infinity =>
        Multiplicity(0,UnlimitedNatural.Infinity)
      case UnlimitedNatural.Finite(n) => Multiplicity(n,UnlimitedNatural.Finite(n))
    })

  def unlimitedNatural: Parser[UnlimitedNatural] =
    ( integer ^^ UnlimitedNatural.Finite
    | STAR ^^^ UnlimitedNatural.Infinity )

  def optionalTyping: Parser[TypeAnnotation] = opt(typing).map(_.getOrElse(TypeAnnotation.Null))

  def typing: Parser[TypeAnnotation] =
    positioned(named("type signature", COLON ~> typeExp ~ opt(multiplicity)) ^^ {
      case nps~mult => TypeAnnotation(nps, mult)
    })

  private def constraintContent(tpe: ConstraintType, name: Option[sysml.SimpleName], until: Elem) = Parser[UnprocessedConstraint] { input =>
    val start = input.pos
    val first = input.offset
    var r = input
    while (!r.atEnd && r.first != until) { r = r.rest }
    val last = r.offset
    val res = UnprocessedConstraint(tpe, name, input.source.subSequence(first + 1,last).toString)
    res.pos = start
    Success(res,r)
  }

  def constraint(tpe: ConstraintType): Parser[UnprocessedConstraint] =
    ((INV ~> opt(simpleName)) <~ COLON) >> { n =>
      ignoreIndentation(captureConstraint(tpe, n, allExcept(INV, DEDENT, SEPARATOR).*))
    }

  def propertyList[T <: TypedElementProperty](ps: Parser[T]*): Parser[Seq[T]] = {
    val combined = ps.foldRight[Parser[T]](failure("expected property")) {
      case (p,alt) => p | alt
    }
    LEFT_BRACE ~! repsep(combined,COMMA) <~ RIGHT_BRACE ^^ (_._2)
  }

  def typedElementProperties: Parser[Seq[TypedElementProperty]] =
    ("seq" | "sequence") ^^^ Seq(TypedElementProperty.Unique(false),TypedElementProperty.Ordered(true))

  def uniqueProperty: Parser[TypedElementProperty.Unique] =
    positioned( "unique" ^^^ TypedElementProperty.Unique(true)
    | "nonunique" ^^^ TypedElementProperty.Unique(false) )

  def orderedProperty: Parser[TypedElementProperty.Ordered] =
    positioned( "ordered" ^^^ TypedElementProperty.Ordered(true)
      | "unordered" ^^^ TypedElementProperty.Ordered(false) )

  def qualifiedName[T <: NamedElement]: Parser[Name] =
    positioned( simpleName[T] ~ (DOT ~> rep1sep(unreservedSimpleName, DOT)) ^^ mkList ^^ (_.map(_.name)) ^^ PathName
      | simpleName[T] )

  def subsetsProperty: Parser[ReferenceProperty.Subsets] =
    positioned( ("subsets" ~! captureConstraint(ConstraintType.Query,None,allExcept(RIGHT_BRACE, COMMA).+)) ^^ { case _~s => ReferenceProperty.Subsets(s) } )

  def queryProperty: Parser[OperationProperty.Query] =
    positioned("query" ^^^ OperationProperty.Query(true))

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
