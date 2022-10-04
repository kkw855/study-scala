ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "study-scala"
  )

scalacOptions += "-language:implicitConversions"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.8.0"

libraryDependencies += "dev.zio" %% "zio" % "2.0.2"
// Test
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test"
