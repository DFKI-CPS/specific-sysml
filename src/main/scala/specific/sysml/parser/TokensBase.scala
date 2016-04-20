package specific.sysml.parser

import scala.util.parsing.combinator.token.Tokens

/**
  * Created by martin on 20.04.16.
  */
trait TokensBase { self: Tokens =>
  abstract class Keyword(val chars: String) extends Token
  abstract class Delimiter(override val chars: String) extends Keyword(chars)
}
