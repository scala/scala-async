scalaVersion := "2.10.3"

// Uncomment to test with a locally built copy of Scala.
// scalaHome := Some(file("/code/scala2/build/pack"))


organization := "org.scala-lang.modules.scala-async"

name := "scala-async"

version := "0.9.0-SNAPSHOT"

libraryDependencies <++= (scalaVersion) {
  sv => Seq(
    // TODO we should make this provided after we rely on @compileTimeOnly in scla-library in 2.11.-
    //      but if we do that now, and a user doesn't have this on the classpath, they can get the
    //      dreaded MissingRequirementErrors when unpickling types from scala.async.Async
    "org.scala-lang" % "scala-reflect" % sv,
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

scalacOptions in compile ++= Seq("-optimize", "-deprecation", "-unchecked", "-Xlint", "-feature")

scalacOptions in Test ++= Seq("-Yrangepos")

// Generate $name.properties to store our version as well as the scala version used to build
resourceGenerators in Compile <+= Def.task {
  val props = new java.util.Properties
  props.put("version.number", version.value)
  props.put("scala.version.number", scalaVersion.value)
  props.put("scala.binary.version.number", scalaBinaryVersion.value)
  val file = (resourceManaged in Compile).value / s"${name.value}.properties"
  IO.write(props, null, file)
  Seq(file)
}

mappings in (Compile, packageBin) += {
   (baseDirectory.value / s"${name.value}.properties") -> s"${name.value}.properties"
}


description := "An asynchronous programming facility for Scala that offers a direct API for working with Futures."

homepage := Some(url("http://github.com/scala/async"))

startYear := Some(2012)

licenses +=("Scala license", url("https://github.com/scala/async/blob/master/LICENSE"))

// Uncomment to disable test compilation.
// (sources in Test) ~= ((xs: Seq[File]) => xs.filter(f => Seq("TreeInterrogation", "package").exists(f.name.contains)))

// maven publishing
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  val repo = if (version.value.trim.endsWith("SNAPSHOT"))
    "snapshots" at nexus + "content/repositories/snapshots"
  else
    "releases" at nexus + "service/local/staging/deploy/maven2"
  Some(repo)
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

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

osgiSettings

val osgiVersion = version(_.replace('-', '.'))

OsgiKeys.bundleSymbolicName := s"${organization.value}.${name.value}"

OsgiKeys.bundleVersion := osgiVersion.value

OsgiKeys.exportPackage := Seq(s"scala.async.*;version=${version.value}")

// Sources should also have a nice MANIFEST file
packageOptions in packageSrc := Seq(Package.ManifestAttributes(
                      ("Bundle-SymbolicName", s"${organization.value}.${name.value}.source"),
                      ("Bundle-Name", s"${name.value} sources"),
                      ("Bundle-Version", osgiVersion.value),
                      ("Eclipse-SourceBundle", s"""${organization.value}.${name.value};version="${osgiVersion.value}";roots:="."""")
                  ))