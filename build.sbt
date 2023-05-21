ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "ChessGame"
  )
libraryDependencies += "com.typesafe.play" %% "play" % "2.9.0-M2"
libraryDependencies += "com.lihaoyi" %% "ujson-circe" % "3.0.0"
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.9.1"