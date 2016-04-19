package specific.sysml

/*trait Model

class Arity(lowerBound: Int, upperBound: Int)
object * extends Arity(0,-1)

object Arity {
  implicit def arityFromInt(n: Int) = new Arity(n,n)
  implicit def arityFromRange(r: Range) = new Arity(r.start, r.end)
}

trait Reference[B <: Block,T <: Block]
trait Attribute[B <: Block,T]
trait Operation[B <: Block,T]

trait Block[T] {
  object Reference {
    def apply[U <: Block](arity: Arity)(opposite: U => Reference[U,T]): Reference[T, U] = ???
  }
}

object ACS extends Model {
  class Building extends Block[Building] {
    val inhabitants = Reference[Person](*)(opposite = _.home)
    val neighbours = Reference[Building](1 to 7)
  }

  class Person extends Block[Person] {
    val home = Reference[Building](1)(opposite = _.inhabitants)
  }
}*/