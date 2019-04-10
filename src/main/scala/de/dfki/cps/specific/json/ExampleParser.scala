package de.dfki.cps.specific.json

import spray.json._
import DefaultJsonProtocol._

object Example extends App {

  // generates an intermediate JSON representation (abstract syntax tree)
  val res = """{ "foo": "bar" }""".parseJson // JsValue = {"foo":"bar"}

  res.convertTo[Map[String, String]] // Map(foo -> bar)
  val values = List("a", "b", "c")
  values.toJson.prettyPrint // ["a", "b", "c"]

  print(res)
}
