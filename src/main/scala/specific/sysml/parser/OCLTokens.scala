package specific.sysml.parser

import specific.uml
import specific.ocl

import scala.collection.immutable.SortedSet
import scala.util.parsing.combinator.token.Tokens

/**
  * Created by martin on 15.04.16.
  */
trait OCLTokens extends TokensBase { self: Tokens =>
  val oclKeywords = SortedSet(
    AND, BODY, CONTEXT, DEF, DERIVE, ELSE, ENDIF, ENDPACKAGE, FALSE, IF,
    IMPLIES, IN, INIT, INV, INVALID, LET, NOT, NULL, OR, PACKAGE, POST,
    PRE, SELF, STATIC, THEN, TRUE, XOR
  )(Ordering.by(_.chars))

  sealed trait OCLKeyword
  case object AND extends Keyword("and") with OCLKeyword
  case object BODY extends Keyword("body") with OCLKeyword
  case object CONTEXT extends Keyword("context") with OCLKeyword
  case object DEF extends Keyword("def") with OCLKeyword
  case object DERIVE extends Keyword("derive") with OCLKeyword
  case object ELSE extends Keyword("else") with OCLKeyword
  case object ENDIF extends Keyword("endif") with OCLKeyword
  case object ENDPACKAGE extends Keyword("endpackage") with OCLKeyword
  case object FALSE extends Keyword("false") with OCLKeyword
  case object IF extends Keyword("if") with OCLKeyword
  case object IMPLIES extends Keyword("implies") with OCLKeyword
  case object IN extends Keyword("in") with OCLKeyword
  case object INIT extends Keyword("init") with OCLKeyword
  case object INV extends Keyword("inv") with OCLKeyword
  case object INVALID extends Keyword("invalid") with OCLKeyword
  case object LET extends Keyword("let") with OCLKeyword
  case object NOT extends Keyword("not") with OCLKeyword
  case object NULL extends Keyword("null") with OCLKeyword
  case object OR extends Keyword("or") with OCLKeyword
  case object PACKAGE extends Keyword("package") with OCLKeyword
  case object POST extends Keyword("post") with OCLKeyword
  case object PRE extends Keyword("pre") with OCLKeyword
  case object SELF extends Keyword("self") with OCLKeyword
  case object STATIC extends Keyword("static") with OCLKeyword
  case object THEN extends Keyword("then") with OCLKeyword
  case object TRUE extends Keyword("true") with OCLKeyword
  case object XOR extends Keyword("xor") with OCLKeyword

  val oclDelimiters = Set(
    PLUS,MINUS,STAR,SLASH,LESS_THAN,GREATER_THAN,NOT_EQUAL,LESS_OR_EQUAL,GREATER_OR_EQUAL,
    CIRCUMFLEX,DOUBLE_CIRCUMFLEX,LEFT_PARENS,RIGHT_PARENS,RIGHT_ARROW,EQUALS,LEFT_SQUARE_BRACKET,
    RIGHT_SQUARE_BRACKET,AT,PIPE,DOUBLE_COLON
  )

  sealed trait OCLDelimiter
  case object AT extends Delimiter("@") with OCLDelimiter
  case object PLUS extends Delimiter("+") with OCLDelimiter
  case object MINUS extends Delimiter("-") with OCLDelimiter
  case object STAR extends Delimiter("*") with OCLDelimiter
  case object SLASH extends Delimiter("/") with OCLDelimiter
  case object LESS_THAN extends Delimiter("<") with OCLDelimiter
  case object GREATER_THAN extends Delimiter(">") with OCLDelimiter
  case object NOT_EQUAL extends Delimiter("<>") with OCLDelimiter
  case object LESS_OR_EQUAL extends Delimiter("<=") with OCLDelimiter
  case object GREATER_OR_EQUAL extends Delimiter(">=") with OCLDelimiter
  case object CIRCUMFLEX extends Delimiter("^") with OCLDelimiter
  case object DOUBLE_CIRCUMFLEX extends Delimiter("^^") with OCLDelimiter
  case object DOUBLE_COLON extends Delimiter("::") with OCLDelimiter
  case object LEFT_PARENS extends Delimiter("(") with OCLDelimiter
  case object RIGHT_PARENS extends Delimiter(")") with OCLDelimiter
  case object RIGHT_ARROW extends Delimiter("->") with OCLDelimiter
  case object EQUALS extends Delimiter("=") with OCLDelimiter
  case object LEFT_SQUARE_BRACKET extends Delimiter("[") with OCLDelimiter
  case object RIGHT_SQUARE_BRACKET extends Delimiter("]") with OCLDelimiter
  case object PIPE extends Delimiter("|") with OCLDelimiter


  val oclReserved = Set(
    "Bag",
    "Boolean",
    "Collection",
    "Integer",
    "OclAny",
    "OclInvalid",
    "OclMessage",
    "OclVoid",
    "OrderedSet",
    "Real",
    "Sequence",
    "Set",
    "String",
    "Tuple",
    "UnlimitedNatural"
  )
}
