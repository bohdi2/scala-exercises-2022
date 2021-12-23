import Dependencies._

ThisBuild / scalaVersion     := "2.13.7"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "org.bodhi"
ThisBuild / organizationName := "bodhi"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq("-deprecation", "-Xfatal-warnings"),
  libraryDependencies ++= Seq(
    scalaTest % Test,
   "org.typelevel" %% "cats-core" % "2.1.0"
  )
)


lazy val root = (project in file("."))
  .settings(
    commonSettings,
    name := "bodhi"
  )
  .aggregate(
    calculator,
    data,
    exercises,
    genericFold,
    genericList,
    intList,
    runLength,
    subGroup,
    typeClasses,
    cats1,
    cats2,
    cats3,
    cats4
  )

lazy val calculator = (project in file("Calculator")).settings(commonSettings  )

lazy val data = (project in file("Data")).settings(commonSettings)

lazy val exercises = (project in file("exercises")).settings(commonSettings)

lazy val genericFold = (project in file("GenericFold")).settings(commonSettings)

lazy val genericList = (project in file("GenericList")).settings(commonSettings)

lazy val intList = (project in file("IntList")).settings(commonSettings)

lazy val runLength = (project in file("RunLength")).settings(commonSettings)
lazy val subGroup = (project in file("SubGroup")).settings(commonSettings)
lazy val typeClasses = (project in file("TypeClasses")).settings(commonSettings)

lazy val cats1 = (project in file("cats_1")).settings(commonSettings)
lazy val cats2 = (project in file("cats_2")).settings(commonSettings)
lazy val cats3 = (project in file("cats_3")).settings(commonSettings)
lazy val cats4 = (project in file("cats_4")).settings(commonSettings)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"

