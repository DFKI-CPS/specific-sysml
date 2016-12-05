organization := "de.dfki.specific"
name := "SysML"
version := "1.0"
scalaVersion := "2.12.0"
scalaVersion in ThisBuild := "2.12.0"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalatest" %% "scalatest" % "3.0.0+" % "test",
  "com.google.inject" % "guice" % "3.0",
  "log4j" % "log4j" % "latest.integration"
) ++ Seq(
  "bundle" % "org.eclipse.core.runtime" % "latest.integration",
  "bundle" % "org.eclipse.papyrus.sysml" % "latest.integration",
  "bundle" % "org.eclipse.ocl.pivot.uml" % "latest.integration",
  "bundle" % "org.eclipse.ocl.uml" % "latest.integration",
  "bundle" % "org.eclipse.uml2.uml" % "latest.integration",
  "bundle" % "org.eclipse.ocl.xtext.essentialocl" % "latest.integration"
).map(_.force())

externalIvySettings()
ivyXML := scala.xml.XML.load((baseDirectory.value / "exclude.xml").toURL)