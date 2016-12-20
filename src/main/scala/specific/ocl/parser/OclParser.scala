package specific.ocl.parser

import specific.ocl.CollectionKind
import specific.ocl.Expressions.VariableDeclaration
import specific.sysml.{Name, NamedElement, PathName, ResolvedName, Types}
import specific.sysml.parser.SysMLLexer
import specific.{ocl, sysml}
import Types.Classifier
import specific.util.ParserHelpers

import scala.util.parsing.combinator.PackratParsers

object OclParsers extends OclParsers

/**
  * Created by martin on 25.04.16.
  */
trait OclParsers extends PackratParsers with ParserHelpers {
  import OclTokens._

  override type Elem = OclLexer.Token

  import OclLexer._
  implicit def elem[T <: Elem](elem: T): Parser[Elem] = accept(elem)

  def oclExpression: Parser[Any] =
    ( callExp
    | variableExp
    | literalExp
    | letExp
    | oclMessageExp
    | ifExp )

  def variableExp: Parser[Any] =
    simpleName | SELF

  def simpleName[T <: NamedElement]: Parser[sysml.SimpleName] = positioned(acceptMatch("simple name", {
    case s: OclTokens.SimpleName if !s.isReserved => sysml.SimpleName(s.chars)
  }))

  def restrictedKeyword = acceptMatch("restricted keyword", {
    case s: OclTokens.SimpleName if s.isReserved => sysml.SimpleName(s.chars)
  })

  def unreservedSimpleName = acceptMatch("unreserved simple name", {
    case s: OclTokens.SimpleName => sysml.SimpleName(s.chars)
  })

  def pathName[T <: NamedElement]: Parser[Name] =
    positioned( simpleName[T] ~ (DOUBLE_COLON ~> rep1sep(unreservedSimpleName, DOUBLE_COLON)) ^^ mkList ^^ (_.map(_.name)) ^^ PathName
    | simpleName[T] )



  def literalExp: Parser[Any] =
    ( enumLiteralExp
    | collectionLiteralExp
    | tupleLiteralExp
    | primitiveLiteralExp
    | typeLiteralExp )

  def enumLiteralExp: Parser[Any] =
    pathName ~ DOUBLE_COLON ~ simpleName

  def collectionLiteralExp: Parser[Any] =
    collectionTypeIdentifier ~ enclosed(LEFT_BRACE, collectionLiteralParts, RIGHT_BRACE)

  def collectionTypeIdentifier: Parser[CollectionKind] =
    ( "Set" ^^^ CollectionKind.Set
    | "Bag" ^^^ CollectionKind.Bag
    | "Sequence" ^^^ CollectionKind.Sequence
    | "Collection" ^^^ CollectionKind.Collection
    | "OrderedSet" ^^^ CollectionKind.OrderedSet )
  
  def collectionLiteralParts = rep1sep(collectionLiteralPart, COMMA)

  def collectionLiteralPart =
    collectionRange | oclExpression

  def collectionRange =
    oclExpression ~ (COMMA ~> oclExpression)

  def primitiveLiteralExp: Parser[Any] =
    ( integerLiteralExp
    | realLiteralExp
    | stringLiteralExp
    | booleanLiteralExp
    | unlimitedNaturalLiteralExp
    | nullLiteralExp
    | invalidLiteralExp )

  def tupleLiteralExp: Parser[Any] = "Tuple" ~> enclosed(LEFT_BRACE, variableDeclarationList, RIGHT_BRACE)

  def unlimitedNaturalLiteralExp = integerLiteralExp | STAR

  def integerLiteralExp = acceptMatch("integer literal",{
    case n: IntegerLiteral => n.value
  })

  def realLiteralExp = acceptMatch("real literal",{
    case r: RealLiteral => r.value
  })

  def stringLiteralExp = acceptMatch("string literal",{
    case s: StringLiteral => s.chars
  })

  def booleanLiteralExp = acceptMatch("boolean literal",{
    case b: BooleanLiteral => b.value
  })

  def typeLiteralExp = typeExp

  def callExp: Parser[Any] = featureCallExp | loopExp

  def loopExp: Parser[Any] = iteratorExp | iterateExp

  def iteratorExp: Parser[Any] = ???

  def iterateExp = oclExpression ~ ((RIGHT_ARROW ~ "iterate") ~>
    enclosed(
      LEFT_PARENS,
      opt(variableDeclaration <~ SEMICOLON) ~ variableDeclaration ~
        (PIPE ~> oclExpression),
      RIGHT_PARENS))

  def variableDeclaration: Parser[VariableDeclaration] =
    simpleName ~ opt(COLON ~> typeExp) ^^ { //~ opt(EQUALS ~> oclExpression)
      case name ~ tpe => VariableDeclaration(name.name,tpe)
    }

  def typeExp: Parser[Name] = positioned(
      pathName
    | collectionType
    | tupleType
    | primitiveType
    | oclType )

  def primitiveType: Parser[ResolvedName[Classifier]] =
    ( "Boolean" ^^^ sysml.Types.Boolean
    | "Integer" ^^^ sysml.Types.Integer
    | "Real" ^^^ sysml.Types.Real
    | "Unit" ^^^ sysml.Types.Unit
    | "String" ^^^ sysml.Types.String
    | "UnlimitedNatural" ^^^ sysml.Types.UnlimitedNatural) ^^ sysml.ResolvedName[Classifier]

  def oclType: Parser[ResolvedName[Classifier]] =
    ( "OclAny" ^^^ ocl.Types.AnyType
    | "OclInvalid" ^^^ ocl.Types.InvalidType
    | "OclMessage" ~! err("OclMessage is currently not supported") ^^^ ocl.Types.InvalidType
    | "OclVoid" ^^^ ocl.Types.VoidType)  ^^ sysml.ResolvedName[Classifier]

  def collectionType: Parser[ResolvedName[Classifier]] =
    collectionTypeIdentifier ~ enclosed(LEFT_PARENS, typeExp, RIGHT_PARENS) ^^ {
      case ct ~ t => sysml.ResolvedName(ocl.Types.collection(ct,t))
    }

  def tupleType: Parser[ResolvedName[Classifier]] =
    "Tuple" ~> enclosed(LEFT_PARENS, variableDeclarationList, RIGHT_PARENS) ^^ ocl.Types.TupleType ^^ sysml.ResolvedName[Classifier]

  def variableDeclarationList: Parser[Seq[VariableDeclaration]] =
    rep1sep(variableDeclaration, COMMA)

  def featureCallExp =
    ( operationCallExp
    | propertyCallExp
    | navigationCallExp )

  def operationCallExp =
    ( simpleName ~ enclosed(LEFT_PARENS, arguments, RIGHT_PARENS)
    | simpleName ~ isMarkedPre ~ enclosed(LEFT_PARENS, arguments, RIGHT_PARENS)
    | pathName ~ enclosed(LEFT_PARENS, arguments, RIGHT_PARENS)
    | simpleName ~ oclExpression
    | oclExpression ~ simpleName ~ oclExpression
    | oclExpression ~ (RIGHT_ARROW ~> simpleName) ~ enclosed(LEFT_PARENS, arguments, RIGHT_PARENS)
    | oclExpression ~ (DOT ~> simpleName) ~ enclosed(LEFT_PARENS, arguments, RIGHT_PARENS)
    | oclExpression ~ (DOT ~> simpleName) ~ isMarkedPre ~ enclosed(LEFT_PARENS, arguments, RIGHT_PARENS)
    | oclExpression ~ (DOT ~> pathName) ~ (DOUBLE_COLON ~> simpleName) ~ enclosed(LEFT_PARENS, arguments, RIGHT_PARENS)
    | oclExpression ~ (DOT ~> pathName) ~ (DOUBLE_COLON ~> simpleName) ~ isMarkedPre ~ enclosed(LEFT_PARENS, arguments, RIGHT_PARENS))

  def propertyCallExp =
    ( oclExpression ~ (DOT ~> simpleName) ~ opt(isMarkedPre)
    | simpleName ~ opt(isMarkedPre)
    | pathName
    | oclExpression ~ (DOT ~> pathName) ~ (DOUBLE_COLON ~> simpleName) ~ opt(isMarkedPre))

  def navigationCallExp =
    ( propertyCallExp
    | associationClassCallExp )

  def associationClassCallExp =
    ( oclExpression ~ (DOT ~> simpleName) ~ opt(enclosed(LEFT_SQUARE_BRACKET, arguments, RIGHT_SQUARE_BRACKET)) ~ opt(isMarkedPre)
    | simpleName ~ opt(enclosed(LEFT_SQUARE_BRACKET, arguments, RIGHT_SQUARE_BRACKET)) ~ opt(isMarkedPre) )

  def isMarkedPre = AT ~ PRE

  def arguments = rep1sep(oclExpression, COMMA)

  def letExp: Parser[Any] = LET ~> variableDeclaration ~ letExpSub

  def letExpSub: Parser[Any] =
    ( COMMA ~> variableDeclaration ~ letExpSub
    | IN ~> oclExpression )

  def oclMessageExp: Parser[Any] =
    ( oclExpression ~ (DOUBLE_CIRCUMFLEX ~> simpleName) ~ enclosed(LEFT_PARENS, oclMessageArguments, RIGHT_PARENS)
    | oclExpression ~ (CIRCUMFLEX ~> simpleName) ~ enclosed(LEFT_PARENS, oclMessageArguments, RIGHT_PARENS) )

  def oclMessageArguments: Parser[Any] = rep1sep(oclMessageArg, COMMA)

  def oclMessageArg: Parser[Any] =
    ( QUESTIONMARK ~ opt(COLON ~ typeExp)
    | oclExpression )

  def ifExp: Parser[Any] =
    (IF ~> oclExpression) ~
    (THEN ~> oclExpression) ~
    (ELSE ~> oclExpression) <~ ENDIF

  def nullLiteralExp =
    NULL

  def invalidLiteralExp =
    INVALID

  //// HELPERS

  protected implicit def keyName(what: String): Parser[String] = acceptMatch(what, {
    case n: OclTokens.SimpleName if n.chars == what => what
  })

  protected implicit def keyName2(what: String): ParserExts[String] = ParserExts(keyName(what))
}
