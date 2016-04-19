package specific.sysml.parser

import scala.collection.immutable.SortedSet
import scala.util.parsing.combinator.token.Tokens

/**
  * Created by martin on 15.04.16.
  */
trait OCLTokens { self: Tokens =>
  val oclKeywords = SortedSet(
    AND, BODY, CONTEXT, DEF, DERIVE, ELSE, ENDIF, ENDPACKAGE, FALSE, IF,
    IMPLIES, IN, INIT, INV, INVALID, LET, NOT, NULL, OR, PACKAGE, POST,
    PRE, SELF, STATIC, THEN, TRUE, XOR
  )(Ordering.by(_.chars))

  sealed abstract class OCLKeyword(val chars: String) extends Token

  case object AND extends OCLKeyword("and")
  case object BODY extends OCLKeyword("body")
  case object CONTEXT extends OCLKeyword("context")
  case object DEF extends OCLKeyword("def")
  case object DERIVE extends OCLKeyword("derive")
  case object ELSE extends OCLKeyword("else")
  case object ENDIF extends OCLKeyword("endif")
  case object ENDPACKAGE extends OCLKeyword("endpackage")
  case object FALSE extends OCLKeyword("false")
  case object IF extends OCLKeyword("if")
  case object IMPLIES extends OCLKeyword("implies")
  case object IN extends OCLKeyword("in")
  case object INIT extends OCLKeyword("init")
  case object INV extends OCLKeyword("inv")
  case object INVALID extends OCLKeyword("invalid")
  case object LET extends OCLKeyword("let")
  case object NOT extends OCLKeyword("not")
  case object NULL extends OCLKeyword("null")
  case object OR extends OCLKeyword("or")
  case object PACKAGE extends OCLKeyword("package")
  case object POST extends OCLKeyword("post")
  case object PRE extends OCLKeyword("pre")
  case object SELF extends OCLKeyword("self")
  case object STATIC extends OCLKeyword("static")
  case object THEN extends OCLKeyword("then")
  case object TRUE extends OCLKeyword("true")
  case object XOR extends OCLKeyword("xor")

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

  case class SimpleName(chars: String) extends Token
}
