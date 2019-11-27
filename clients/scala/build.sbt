name := "aicup2019"

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.10"

lazy val app = (project in file("app")).
  settings(
    mainClass in assembly := Some("Runner"),
    assemblyJarName in assembly := "aicup2019"
  )