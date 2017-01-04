package de.dfki.cps.specific.util

import scala.collection.GenTraversableLike
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

object every {
  def apply[T](implicit classTag: ClassTag[T]): PartialFunction[Any,T] = {
    case x: T => x
  }
}