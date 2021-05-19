val scala3Version = "3.0.0-RC3"


lazy val root = project
  .in(file("."))
  .settings(
    name := "differentiable-rescala",
    version := "0.1.0",

    scalaVersion := scala3Version,
  )
