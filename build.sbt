ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "ChessGame"
  )

libraryDependencies += "com.typesafe.play" %% "play" % "2.9.0-M2"
libraryDependencies += "com.lihaoyi" %% "ujson-circe" % "3.0.0"
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.9.1"
libraryDependencies += "org.scalafx" %% "scalafx" % "16.0.0-R24"

libraryDependencies ++= Seq( // I am using java 11, change it to your version to run the App
  "org.openjfx" % "javafx-base"     % "11" % "compile",
  "org.openjfx" % "javafx-controls" % "11" % "compile",
  "org.openjfx" % "javafx-graphics" % "11" % "compile",
  "org.openjfx" % "javafx-fxml"     % "11" % "compile",
  "org.openjfx" % "javafx-media"    % "11" % "compile",
  "org.openjfx" % "javafx-web"      % "11" % "compile",
  "org.openjfx" % "javafx-swing"    % "11" % "compile"
)

