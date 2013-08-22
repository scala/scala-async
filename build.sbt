scalaVersion := "2.10.3-RC1"

organization := "org.typesafe.async" // TODO new org name under scala-lang.

// Uncomment to test with a locally built copy of Scala.
// scalaHome := Some(file("/code/scala2/build/pack"))

name := "scala-async"

version := "1.0.0-SNAPSHOT"

libraryDependencies <++= (scalaVersion) {
  sv => Seq(
    "org.scala-lang" % "scala-reflect" % sv % "provided",
    "org.scala-lang" % "scala-compiler" % sv % "provided"
  )
}

libraryDependencies += "junit" % "junit-dep" % "4.10" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test"

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s")

parallelExecution in Global := false

autoCompilerPlugins := true

scalacOptions ++= (scalaHome.value match {
  case Some(sh) =>
    // Use continuations plugin from the local scala instance
    val continuationsJar = sh / "misc" / "scala-devel" / "plugins" / "continuations.jar"
    ("-Xplugin:" + continuationsJar.getAbsolutePath) :: Nil
  case None =>
    Nil
})

libraryDependencies ++= (scalaHome.value match {
  case Some(sh) =>
    Nil
  case None =>
    // Use continuations plugin from the published artifact.
    compilerPlugin("org.scala-lang.plugins" % "continuations" % scalaVersion.value) :: Nil
})

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
