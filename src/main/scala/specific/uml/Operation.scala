package specific.uml

import specific.uml.Types.Classifier

import scala.concurrent.Promise

/**
  * Created by martin on 25.04.16.
  */
abstract class Operation(
  val isOrdered: Boolean,
  val isQuery: Boolean,
  val isUnique: Boolean,
  val lower: BigInt,
  val upper: UnlimitedNatural,
  val typeName: String)

trait OperationContext {
  val typed = Promise[Classifier]
  val owner = Promise[Classifier]
}