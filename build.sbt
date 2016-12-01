externalIvySettings()

name := "SysML"
version := "1.0"
scalaVersion := "2.12.0"
scalaVersion in ThisBuild := "2.12.0"

ivyPaths :=  new IvyPaths((baseDirectory in ThisBuild).value, Some((baseDirectory in ThisBuild).value / "ivy-cache"))

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

libraryDependencies ++= Seq(
  "bundle" % "javax.inject" % "1.0.0+",
  "bundle" % "org.eclipse.emf.common" % "2.12.0+",
  "bundle" % "org.eclipse.emf.ecore" % "2.12.0+",
  "bundle" % "org.eclipse.emf.ecore.xmi" % "2.12.0+",
  "bundle" % "org.eclipse.uml2.types" % "2.0.0+",
  "bundle" % "org.eclipse.uml2.common" % "2.1.0+",
  "bundle" % "org.eclipse.uml2.uml.profile.standard" % "1.0+",
  /*"bundle" % "org.eclipse.ocl.common" % "1.4.+",
  "bundle" % "org.eclipse.uml2.common" % "2.1.+",
  "bundle" % "org.eclipse.uml2.uml" % "5.2.+",
  "bundle" % "org.eclipse.emf.common" % "2.12.0",
  "bundle" % "org.eclipse.emf.pivot" % "1.0.+",
  "bundle" % "org.eclipse.ocl.pivot.uml" % "1.0.+",
  "bundle" % "org.eclipse.core.runtime" % "3.12.+",*/
  "bundle" % "org.eclipse.papyrus.sysml" % "1.2.+"
).map(_.force().excludeAll(Seq(
  //"org.eclipse.uml2.codegen.ecore"
  "org.eclipse.eml.ecore.util",
  "org.eclipse.emf.mapping.ecore2xml",
  "org.eclipse.core.resources",
  "org.eclipse.core.runtime",
  "javax.crypto",
  "javax.crypto.spec",
  "org.eclipse.equinox.registry",
  "org.eclipse.equinox.common",
  "org.osgi.framework"
).flatMap(a => Seq(ExclusionRule("package",a),ExclusionRule("bundle",a))):_*))