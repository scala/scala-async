scalaVersion := "2.10.1"

// Uncomment to test with a locally built copy of Scala.
// scalaHome := Some(file("/code/scala2/build/pack"))

organization := "org.typesafe.async" // TODO new org name under scala-lang.

name := "scala-async"

version := "1.0.0-SNAPSHOT"

libraryDependencies <++= (scalaVersion) {
  sv => Seq(
    "org.scala-lang" % "scala-reflect" % sv,
    "org.scala-lang" % "scala-compiler" % sv % "test"
  )
}

libraryDependencies += "junit" % "junit-dep" % "4.10" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10-M2" % "test"

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s")

parallelExecution in Global := false

autoCompilerPlugins := true

libraryDependencies <++= (scalaVersion, scalaHome) {
  (ver, sh) =>
    sh match {
      case Some(sh) =>
        Nil
      case None =>
        compilerPlugin("org.scala-lang.plugins" % "continuations" % ver) :: Nil
    }
}

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xlint", "-feature")

scalacOptions += "-P:continuations:enable"

scalacOptions <++= scalaHome map {
  case Some(sh) =>
    val continuationsJar = (sh / "misc" / "scala-devel" / "plugins" / "continuations.jar")
    ("-Xplugin:" + continuationsJar.getAbsolutePath) :: Nil
  case None =>
    Nil
}

description := "An asynchronous programming facility for Scala, in the spirit of C# await/async"

homepage := Some(url("http://github.com/scala/async"))

startYear := Some(2012)

licenses +=("Scala license", url("https://github.com/scala/async/blob/master/LICENSE"))

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

// Run ;gen-idea;patch-idea to point the generated config files for IntelliJ
// to use the libraries and sources from `scalaHome`, if defined above.
TaskKey[Unit]("patch-idea") <<= (baseDirectory, scalaHome, scalaVersion) map { (bd, shOpt, sv) =>
  import java.nio.charset.Charset._
  shOpt foreach { sh =>
    Seq("library", "compiler", "reflect") foreach { jar =>
      Seq("_test", "") foreach { suffix =>
        val fileName = "SBT__org_scala_lang_scala_" + jar + "_" + sv.replaceAllLiterally(".", "_") + suffix + ".xml"
        val f = bd / ".idea" / "libraries" / fileName
        if (f.exists) {
          val origContent = IO.read(bd / ".idea" / "libraries" / fileName)
          val origSource = "jar://$USER_HOME$/.ivy2/cache/org.scala-lang/scala-" + jar + "/srcs/scala-" + jar + "-" + sv + "-sources.jar!/"
          // Three '/' required by IntelliJ here for some reason.
          val newSource = "file:///" + (sh.getParentFile.getParentFile / "src" / jar).getAbsolutePath
          val origLib = jar match {
            case "reflect" => "jar://$USER_HOME$/.ivy2/cache/org.scala-lang/scala-reflect/jars/scala-reflect-" + sv + ".jar!/"
            case _        => "jar://$USER_HOME$/.sbt/boot/scala-" + sv + "/lib/scala-" + jar + ".jar!/"
          }
          val newLib = (sh / "lib" / ("scala-" + jar + ".jar")).toURI.toString
          val newContent = origContent.replaceAllLiterally(origSource, newSource).replaceAllLiterally(origLib, newLib)
          IO.write(f, newContent, forName("UTF-8"), append = false)
        }
      }
    }
  }
}
