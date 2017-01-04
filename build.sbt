organization := "net.flatmap"
name := "sysml"
version := "0.1"
scalaVersion := "2.12.1"
scalaVersion in ThisBuild := "2.12.1"

resolvers += Resolver.bintrayRepo("dfki-cps", "maven")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
  "org.scalatest" %% "scalatest" % "3.0.0+" % "test",
  "de.dfki.cps" % "specific-dependencies" % "4.6.0",
  "log4j" % "log4j" % "1.2.17"
)