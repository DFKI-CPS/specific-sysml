package specific.sysml.synthesis

import scala.util.parsing.input.Position

sealed trait Message
case class Error(pos: Position, message: String) extends Message {

}