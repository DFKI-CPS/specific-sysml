organization := "de.dfki.specific"
name := "SysML"
version := "1.0"
scalaVersion := "2.12.0"
scalaVersion in ThisBuild := "2.12.0"

resolvers += Resolver.url("eclipse-updates",url("http://download.eclipse.org/eclipse/updates/4.6/R-4.6.1-201609071200/plugins/"))(
                                                                                                                                   Patterns("[organisation].[module]_[revision].[ext]")
                                                                                                                                 )

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalatest" %% "scalatest" % "3.0.0+" % "test"
) ++ Seq(
  "bundle" % "org.eclipse.papyrus.sysml" % "latest.integration",
  "bundle" % "org.eclipse.ocl.uml" % "latest.integration"
).map(_.force())

externalIvySettings()
ivyXML := scala.xml.XML.load((baseDirectory.value / "exclude.xml").asURL)