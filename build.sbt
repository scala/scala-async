val sharedSettings = ScalaModulePlugin.scalaModuleSettings ++ ScalaModulePlugin.scalaModuleOsgiSettings ++ Seq(
  name := "scala-async",
  scalaModuleAutomaticModuleName := Some("scala.async"),

  crossScalaVersions := Seq("2.13.8", "2.12.16"),
  scalaVersion := crossScalaVersions.value.head,

  OsgiKeys.exportPackage := Seq(s"scala.async.*;version=${version.value}"),

  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
  libraryDependencies += "junit" % "junit" % "4.13.2" % Test,
  libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % Test,

  ScalaModulePlugin.enableOptimizer,
  testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s"),
  Test / scalacOptions ++= Seq("-Yrangepos"),
  scalacOptions ++= List("-deprecation" , "-Xasync"),
)

lazy val proj = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("."))
  .settings(sharedSettings)
  // until we have actually published for Scala.js. this is also why,
  // for now, release.yml does just `proj/versionCheck` instead of `versionCheck`
  .jvmSettings(versionPolicyIntention := Compatibility.BinaryAndSourceCompatible)
  .jsSettings(versionPolicyIntention := Compatibility.None)
  // override sbt-scala-module default (which is unsuitable for Scala.js)
  .jsSettings(Test / fork := false)

lazy val root = project.in(file("."))
  .settings(sharedSettings)
  .aggregate(proj.jvm, proj.js)

Global / parallelExecution := false

// Uncomment to disable test compilation.
// Test / sources ~= ((xs: Seq[File]) => xs.filter(f => Seq("TreeInterrogation", "package").exists(f.name.contains)))

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

commands += testDeterminism

def testDeterminism = Command.command("testDeterminism") { state =>
  val extracted = Project.extract(state)
  println("Running test:clean")
  val (state1, _) = extracted.runTask(LocalRootProject / Test / clean, state)
  println("Running test:compile")
  val (state2, _) = extracted.runTask(LocalRootProject / Test / compile, state1)
  val testClasses = extracted.get(Test / classDirectory)
  val baseline: File = testClasses.getParentFile / (testClasses.getName + "-baseline")
  baseline.mkdirs()
  IO.copyDirectory(testClasses, baseline, overwrite = true)
  IO.delete(testClasses)
  println("Running test:compile")
  val (state3, _) = extracted.runTask(LocalRootProject / Test / compile, state2)

  import java.nio.file.FileVisitResult
  import java.nio.file.{Files, Path}
  import java.nio.file.SimpleFileVisitor
  import java.nio.file.attribute.BasicFileAttributes
  import java.util

  def checkSameFileContents(one: Path, other: Path): Unit = {
    Files.walkFileTree(one, new SimpleFileVisitor[Path]() {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        val result: FileVisitResult = super.visitFile(file, attrs)
        // get the relative file name from path "one"
        val relativize: Path = one.relativize(file)
        // construct the path for the counterpart file in "other"
        val fileInOther: Path = other.resolve(relativize)
        val otherBytes: Array[Byte] = Files.readAllBytes(fileInOther)
        val thisBytes: Array[Byte] = Files.readAllBytes(file)
        if (!(util.Arrays.equals(otherBytes, thisBytes))) {
          throw new AssertionError(file + " is not equal to " + fileInOther)
        }
        return result
      }
    })
  }
  println("Comparing: " + baseline.toPath + " and " + testClasses.toPath)
  checkSameFileContents(baseline.toPath, testClasses.toPath)
  checkSameFileContents(testClasses.toPath, baseline.toPath)

  state3
}
