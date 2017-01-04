package de.dfki.cps.specific.ocl.parser

import scala.collection.immutable.SortedSet
import scala.util.parsing.combinator.token.Tokens

/**
  * Created by martin on 15.04.16.
  */
object OclTokens extends Tokens {
  val oclKeywords = SortedSet(
    AND, BODY, CONTEXT, DEF, DERIVE, ELSE, ENDIF, ENDPACKAGE, FALSE, IF,
    IMPLIES, IN, INIT, INV, INVALID, LET, NOT, NULL, OR, PACKAGE, POST, PRE,
    SELF, STATIC, THEN, TRUE, XOR
  )(Ordering.by(_.chars))

  sealed abstract class Keyword(val chars: String) extends Token
  case object AND extends Keyword("and")
  case object BODY extends Keyword("body")
  case object CONTEXT extends Keyword("context")
  case object DEF extends Keyword("def")
  case object DERIVE extends Keyword("derive")
  case object ELSE extends Keyword("else")
  case object ENDIF extends Keyword("endif")
  case object ENDPACKAGE extends Keyword("endpackage")
  case object FALSE extends BooleanLiteral(false)
  case object IF extends Keyword("if")
  case object IMPLIES extends Keyword("implies")
  case object IN extends Keyword("in")
  case object INIT extends Keyword("init")
  case object INV extends Keyword("inv")
  case object INVALID extends Keyword("invalid")
  case object LET extends Keyword("let")
  case object NOT extends Keyword("not")
  case object NULL extends Keyword("null")
  case object OR extends Keyword("or")
  case object PACKAGE extends Keyword("package")
  case object POST extends Keyword("post")
  case object PRE extends Keyword("pre")
  case object SELF extends Keyword("self")
  case object STATIC extends Keyword("static")
  case object THEN extends Keyword("then")
  case object TRUE extends BooleanLiteral(true)
  case object XOR extends Keyword("xor")

  val delimiters = SortedSet[Delimiter](
    AT, CIRCUMFLEX, COLON, COMMA, DOUBLE_CIRCUMFLEX, DOUBLE_COLON, DOT, EQUALS,
    GREATER_OR_EQUAL, GREATER_THAN, LEFT_BRACE, LEFT_PARENS,
    LEFT_SQUARE_BRACKET, LESS_OR_EQUAL, LESS_THAN, MINUS, NOT_EQUAL, PIPE, PLUS,
    RIGHT_ARROW, RIGHT_BRACE, RIGHT_PARENS, RIGHT_SQUARE_BRACKET, SEMICOLON,
    SLASH, STAR, QUESTIONMARK
  ) (Ordering.by[Delimiter,(Int,String)](k => (k.chars.length, k.chars)).reverse)

  abstract class Delimiter(val chars: String) extends Token
  case object AT extends Delimiter("@")
  case object CIRCUMFLEX extends Delimiter("^")
  case object COLON extends Delimiter(":")
  case object COMMA extends Delimiter(",")
  case object DOUBLE_CIRCUMFLEX extends Delimiter("^^")
  case object DOUBLE_COLON extends Delimiter("::")
  case object DOT extends Delimiter(".")
  case object EQUALS extends Delimiter("=")
  case object GREATER_OR_EQUAL extends Delimiter(">=")
  case object GREATER_THAN extends Delimiter(">")
  case object LEFT_BRACE extends Delimiter("{")
  case object LEFT_PARENS extends Delimiter("(")
  case object LEFT_SQUARE_BRACKET extends Delimiter("[")
  case object LESS_OR_EQUAL extends Delimiter("<=")
  case object LESS_THAN extends Delimiter("<")
  case object MINUS extends Delimiter("-")
  case object NOT_EQUAL extends Delimiter("<>")
  case object PIPE extends Delimiter("|")
  case object PLUS extends Delimiter("+")
  case object RIGHT_ARROW extends Delimiter("->")
  case object RIGHT_BRACE extends Delimiter("}")
  case object RIGHT_PARENS extends Delimiter(")")
  case object RIGHT_SQUARE_BRACKET extends Delimiter("]")
  case object SEMICOLON extends Delimiter(";")
  case object SLASH extends Delimiter("/")
  case object STAR extends Delimiter("*")
  case object QUESTIONMARK extends Delimiter("?")

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

  case class SimpleName(chars: String) extends Token {
    def isReserved = oclReserved.contains(chars)
  }

  sealed abstract class BooleanLiteral(val value: Boolean) extends Keyword(value.toString)

  case class StringLiteral(chars: String) extends Token

  case class IntegerLiteral(value: BigInt) extends Token {
    def chars = value.toString
  }

  case class RealLiteral(value: BigDecimal) extends Token {
    def chars = value.toString
  }
}
