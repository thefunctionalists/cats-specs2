name := "cats-specs2"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.spire-math" %% "cats-macros" % "0.3.0",
  "org.spire-math" %% "cats-core" % "0.3.0",
  "org.specs2" %% "specs2-core" % "3.6.6"
)

scalacOptions in Test ++= Seq("-Yrangepos")
