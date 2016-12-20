organization := "net.flatmap"
name := "sysml"
version := "0.1"
scalaVersion := "2.12.1"
scalaVersion in ThisBuild := "2.12.1"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalatest" %% "scalatest" % "3.0.0+" % "test"
) ++ Seq(
  "bundle" % "org.eclipse.papyrus.sysml" % "latest.integration",
  "bundle" % "org.eclipse.ocl.uml" % "latest.integration"
).map(_.force())

externalIvySettings()

ivyXML := scala.xml.XML.load((baseDirectory.value / "exclude.xml").asURL)