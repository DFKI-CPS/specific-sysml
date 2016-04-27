package specific.sysml

sealed abstract class UnlimitedNatural(val value: BigInt) {
  require(value > -2, s"invalid argument: $value")
  def isInfinity = value < 0
}
object UnlimitedNatural {
  case class Finite(override val value: BigInt) extends UnlimitedNatural(value) {
    override def toString = value.toString
  }
  case object Infinity extends UnlimitedNatural(-1) {
    override def toString = "*"
  }
}