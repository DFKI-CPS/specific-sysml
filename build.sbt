organization := "de.dfki.cps"
name := "specific-sysml"
version := "0.2.4"
scalaVersion := "2.11.8"
licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
bintrayOrganization := Some("dfki-cps")

scalacOptions := Seq("-deprecation")

crossScalaVersions := Seq("2.11.8","2.12.1")

resolvers += Resolver.bintrayRepo("dfki-cps", "maven")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
  "org.scalatest" %% "scalatest" % "3.0.0+" % "test",
  "de.dfki.cps" % "specific-dependencies" % "4.6.4"
)