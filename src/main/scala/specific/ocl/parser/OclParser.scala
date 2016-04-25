package specific.ocl.parser

import specific.ocl.CollectionKind
import specific.sysml.parser.SysMLLexer
import specific.util.ParserHelpers

import scala.util.parsing.combinator.Parsers

object OclParsers extends OclParsers

/**
  * Created by martin on 25.04.16.
  */
trait OclParsers extends Parsers with ParserHelpers {
  override type Elem = OclLexer.Token

  import OclLexer._

  def oclExpression: Parser[Any] =
    ( callExp
    | variableExp
    | literalExp
    | letExp
    | oclMessageExp
    | ifExp )

  def variableExp =
    simpleName | SELF

  def simpleName = acceptMatch("simple name", {
    case s: OclLexer.SimpleName if !s.isReserved => s
  })

  def restrictedKeyword = acceptMatch("restricted keyword", {
    case s: OclLexer.SimpleName if s.isReserved => s
  })

  def unreservedSimpleName = acceptMatch("unreserved simple name", {
    case s: OclLexer.SimpleName => s
  })

  def pathName =
    ( simpleName
    | simpleName ~ (DOUBLE_COLON ~> rep1sep(unreservedSimpleName, DOUBLE_COLON)) )

  def literalExp =
    ( enumLiteralExp
    | collectionLiteralExp
    | tupleLiteralExp
    | primitiveLiteralExp
    | typeLiteralExp )

  def enumLiteralExp =
    pathName ~ DOUBLE_COLON ~ simpleName

  def collectionLiteralExp =
    collectionTypeIdentifier ~ enclosed(LEFT_BRACE, collectionLiteralParts, RIGHT_BRACE)

  def collectionTypeIdentifier =
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

  def tupleLiteralExp = "Tuple" ~> enclosed(LEFT_BRACE, variableDeclarationList, RIGHT_BRACE)

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

  def variableDeclaration =
    simpleName ~ opt(COLON ~> typeExp) ~ opt(EQUALS ~> oclExpression)

  def typeExp: Parser[Any] =
    ( pathName
    | collectionType
    | tupleType
    | primitiveType
    | oclType )

  def primitiveType =
    ( "Boolean"
    | "Integer"
    | "Real"
    | "String"
    | "UnlimitedNatural" )

  def oclType =
    ( "OclAny"
    | "OclInvalid"
    | "OclMessage"
    | "OclVoid" )

  def collectionType =
    collectionTypeIdentifier ~ enclosed(LEFT_PARENS, typeExp, RIGHT_PARENS)

  def tupleType =
    "Tuple" ~> enclosed(LEFT_PARENS, variableDeclarationList, RIGHT_PARENS)

  def variableDeclarationList =
    rep1sep(variableDeclaration, COMMA)

  def featureCallExp =
    ( operationCallExp
    | propertyCallExp
    | navigationCallExp )

  def operationCallExp =
    ( oclExpression ~ simpleName ~ oclExpression
    | oclExpression ~ (RIGHT_ARROW ~> simpleName) ~ enclosed(LEFT_PARENS, arguments, RIGHT_PARENS)
    | oclExpression ~ (DOT ~> simpleName) ~ enclosed(LEFT_PARENS, arguments, RIGHT_PARENS)
    | simpleName ~ enclosed(LEFT_PARENS, arguments, RIGHT_PARENS)
    | oclExpression ~ (DOT ~> simpleName) ~ isMarkedPre ~ enclosed(LEFT_PARENS, arguments, RIGHT_PARENS)
    | simpleName ~ isMarkedPre ~ enclosed(LEFT_PARENS, arguments, RIGHT_PARENS)
    | pathName ~ enclosed(LEFT_PARENS, arguments, RIGHT_PARENS)
    | simpleName ~ oclExpression
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

  def letExp = LET ~> variableDeclaration ~ letExpSub

  def letExpSub: Parser[Any] =
    ( COMMA ~> variableDeclaration ~ letExpSub
    | IN ~> oclExpression )

  def oclMessageExp =
    ( oclExpression ~ (DOUBLE_CIRCUMFLEX ~> simpleName) ~ enclosed(LEFT_PARENS, oclMessageArguments, RIGHT_PARENS)
    | oclExpression ~ (CIRCUMFLEX ~> simpleName) ~ enclosed(LEFT_PARENS, oclMessageArguments, RIGHT_PARENS) )

  def oclMessageArguments = rep1sep(oclMessageArg, COMMA)

  def oclMessageArg =
    ( QUESTIONMARK ~ opt(COLON ~ typeExp)
    | oclExpression )

  def ifExp =
    (IF ~> oclExpression) ~
    (THEN ~> oclExpression) ~
    (ELSE ~> oclExpression) <~ ENDIF

  def nullLiteralExp =
    NULL

  def invalidLiteralExp =
    INVALID

  //// HELPERS

  protected implicit def keyName(what: String): Parser[String] = acceptMatch(what, {
    case n: SysMLLexer.SimpleName if n.chars == what => what
  })
}
