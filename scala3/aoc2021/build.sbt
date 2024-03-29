val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc2021",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "org.typelevel" %% "cats-parse" % "0.3.6",
    )
  )
