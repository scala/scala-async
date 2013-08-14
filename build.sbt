scalaVersion := "2.10.2"

organization := "org.typesafe.async" // TODO new org name under scala-lang.

name := "scala-async"

version := "1.0.0-SNAPSHOT"

libraryDependencies <++= (scalaVersion) {
  sv => Seq(
    "org.scala-lang" % "scala-reflect" % sv % "provided",
    "org.scala-lang" % "scala-compiler" % sv % "provided"
  )
}

libraryDependencies += "junit" % "junit-dep" % "4.10" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10-M2" % "test"

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s")

parallelExecution in Global := false

autoCompilerPlugins := true

libraryDependencies <<= (scalaVersion, libraryDependencies) {
  (ver, deps) =>
    deps :+ compilerPlugin("org.scala-lang.plugins" % "continuations" % ver)
}

scalacOptions += "-P:continuations:enable"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xlint", "-feature")

scalacOptions in Test ++= Seq("-Yrangepos")

description := "An asynchronous programming facility for Scala, in the spirit of C# await/async"

homepage := Some(url("http://github.com/scala/async"))

startYear := Some(2012)

licenses +=("Scala license", url("https://github.com/scala/async/blob/master/LICENSE"))

// Uncomment to disable test compilation.
// (sources in Test) ~= ((xs: Seq[File]) => xs.filter(f => Seq("TreeInterrogation", "package").exists(f.name.contains)))

pomExtra := (
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
      <timezone>+1</timezone>
      <url>http://github.com/retronym</url>
    </developer>
  </developers>
    <scm>
      <url>git@github.com:scala/async.git/</url>
      <connection>scm:git:git@github.com:scala/async.git</connection>
    </scm>
  )
