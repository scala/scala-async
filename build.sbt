scalaVersion := "2.10.0-RC1"

organization := "org.typesafe.async"

name := "scala-async"

version := "0.1-SNAPSHOT"

libraryDependencies <++= (scalaVersion) {
  sv => Seq(
    "org.scala-lang" % "scala-reflect" % sv,
    "org.scala-lang" % "scala-compiler" % sv % "test"
  )
}

libraryDependencies += "junit" % "junit-dep" % "4.10" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10-M2" % "test"

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s")

autoCompilerPlugins := true

libraryDependencies <<= (scalaVersion, libraryDependencies) {
  (ver, deps) =>
    deps :+ compilerPlugin("org.scala-lang.plugins" % "continuations" % ver)
}

scalacOptions += "-P:continuations:enable"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xlint")

description := "An asynchronous programminig facility for Scala, in the spirit of C# await/async"

homepage := Some(url("http://github.com/phaller/scala-async"))

startYear := Some(2012)

licenses +=("Scala license", url("http://github.com/phaller/scala-async/LICENCE"))

pomExtra := (
  <developers>
    <developer>
      <id>phaller</id>
      <name>Phillipe Haller</name>
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
      <url>git@github.com:phaller/scala-async.git/</url>
      <connection>scm:git:git@github.com:phaller/scala-async.git</connection>
    </scm>
  )
