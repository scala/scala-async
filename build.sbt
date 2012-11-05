scalaVersion := "2.10.0-RC1"

organization := "org.typesafe.async"

name := "scala-async"

version := "0.1-SNAPSHOT"

libraryDependencies <++= (scalaVersion){ sv => Seq(
  "org.scala-lang" % "scala-reflect" % sv,
  "org.scala-lang" % "scala-compiler" % sv % "test"
  )
}

libraryDependencies += "junit" % "junit-dep" % "4.10" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10-M2" % "test"

// TODO scalac / javac options
// TODO metadata
