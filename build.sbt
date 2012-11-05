scalaVersion := "2.10.0-RC1"

organization := "org.typesafe.async"

name := "scala-async"

version := "0.1-SNAPSHOT"

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
