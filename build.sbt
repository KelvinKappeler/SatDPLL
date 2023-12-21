val scala3Version = "3.3.0"

Compile / run / mainClass := Some("Main")

lazy val root = project
  .in(file("."))
  .enablePlugins(StainlessPlugin)
  .settings(
    name := "SatDPLL",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
