package specific.sysml.parser

import org.scalatest.{FlatSpec, Matchers}

import scala.util.parsing.input.CharSequenceReader

/**
  * Created by martin on 15.04.16.
  *
class ScannerTest extends FlatSpec with Matchers {
  val scanner = new Lexer

  "simpleNameCS" must "accept the examples from OCL specificaton" in {
    val examples = Set("String", "i3", "αρετη", "MAX_VALUE", "isLetterOrDigit", "_'true'", "_'>='", "_'\\''")
    examples.foreach { example =>
      val result = scanner.simpleName(new CharSequenceReader(example))
      result match {
        case scanner.Failure(message, input) =>
          fail(s"$message (for $example)")
        case scanner.Success(result, rest) =>
          assert(rest.atEnd)
      }
    }
  }

  it must "fail on invalid input" in {
    val examples = Set("/12", "&", "Ha\\llo", "129", "\"String\"")
    examples.foreach { example =>
      val result = scanner.phrase(scanner.simpleName)(new CharSequenceReader(example))
      result match {
        case scanner.Failure(message, input) =>
          //assert(input.offset == 0)
        case scanner.Success(result, rest) =>
          fail(s"parsed '$example'")
      }
    }
  }
}
*/