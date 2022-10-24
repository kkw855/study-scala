ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "study-scala"
  )

scalacOptions += "-language:implicitConversions"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.8.0"

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.14"
// Test
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test"
