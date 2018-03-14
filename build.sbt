import ScalaModulePlugin._

scalaModuleSettings

scalaVersionsByJvm in ThisBuild := {
  val v211 = "2.11.12"
  val v212 = "2.12.4"
  val v213 = "2.13.0-M3"

  Map(
    7 -> List(v211 -> false),
    8 -> List(v212 -> true, v213 -> true, v211 -> true),
    9 -> List(v212 -> false, v213 -> false, v211 -> false))
}

name := "scala-async"
repoName := "async"

version := "0.9.8-SNAPSHOT"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "test" // for ToolBox
libraryDependencies += "junit" % "junit" % "4.12" % "test"
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

enableOptimizer
testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s")
scalacOptions in Test ++= Seq("-Yrangepos")

parallelExecution in Global := false

// Uncomment to disable test compilation.
// (sources in Test) ~= ((xs: Seq[File]) => xs.filter(f => Seq("TreeInterrogation", "package").exists(f.name.contains)))

description := "An asynchronous programming facility for Scala that offers a direct API for working with Futures."
homepage := Some(url("http://github.com/scala/async"))
startYear := Some(2012)
pomExtra := (
  <issueManagement>
    <system>GitHub</system>
    <url>https://github.com/scala/async/issues</url>
  </issueManagement>
  <developers>
    <developer>
      <id>phaller</id>
      <name>Philipp Haller</name>
      <timezone>+1</timezone>
      <url>http://github.com/phaller</url>
    </developer>
    <developer>
      <id>retronym</id>
      <name>Jason Zaugg</name>
      <timezone>+10</timezone>
      <url>http://github.com/retronym</url>
    </developer>
  </developers>
  )
OsgiKeys.exportPackage := Seq(s"scala.async.*;version=${version.value}")
