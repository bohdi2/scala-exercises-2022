import Dependencies._

ThisBuild / scalaVersion     := "2.13.7"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "org.bodhi"
ThisBuild / organizationName := "bodhi"

lazy val root = (project in file("."))
  .settings(
    name := "bodhi",
    scalacOptions ++= Seq("-deprecation", "-Xfatal-warnings"),
    libraryDependencies += scalaTest % Test
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
