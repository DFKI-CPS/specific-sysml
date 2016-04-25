package specific.uml

sealed abstract class UnlimitedNatural(val value: BigInt) {
  require(value > -2, s"invalid argument: $value")
  def isInfinity = value < 0
}
object UnlimitedNatural {
  case class Finite(override val value: BigInt) extends UnlimitedNatural(value)
  case object Infinity extends UnlimitedNatural(-1)
}