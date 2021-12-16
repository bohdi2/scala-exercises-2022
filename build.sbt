import Dependencies._

ThisBuild / scalaVersion     := "2.13.7"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "org.bodhi"
ThisBuild / organizationName := "bodhi"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq("-deprecation", "-Xfatal-warnings"),
  libraryDependencies += scalaTest % Test
)


lazy val root = (project in file("."))
  .settings(
    commonSettings,
    name := "bodhi",
  )
  .aggregate(
    calculator,
    data,
    exercises,
    genericFold,
    genericList,
    intList)

lazy val calculator = (project in file("Calculator"))
  .settings(
    commonSettings,
    name := "exercises",
  )

lazy val data = (project in file("Data"))
  .settings(
    commonSettings,
    name := "exercises",
  )

lazy val exercises = (project in file("exercises"))
  .settings(
    commonSettings,
  )

lazy val genericFold = (project in file("GenericFold"))
  .settings(
    commonSettings,
  )

lazy val genericList = (project in file("GenericList"))
  .settings(
    commonSettings,
  )

lazy val intList = (project in file("IntList"))
  .settings(
    commonSettings,
    name := "exercises",
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"

