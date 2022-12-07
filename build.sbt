import sbt.Keys.version

lazy val commonSettings = Seq(
  scalaVersion := "2.13.1",
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.9.0",
    "org.tpolecat" %% "atto-core" % "0.7.0",
    "com.softwaremill.quicklens" %% "quicklens" % "1.9.0",
    "org.scalactic" %% "scalactic" % "3.1.0",
    "org.scalatest" %% "scalatest" % "3.1.0" % "test"
  )
)

lazy val root = (project in file("."))
  .settings(
    name := "aoc2022",
    version := "0.1",
    scalaVersion := "2.13.1"
  )

lazy val dayOne = (project in file("day-01"))
  .settings(
    name := "day one",
    commonSettings
  )

lazy val dayTwo = (project in file("day-02"))
  .settings(
    name := "day two",
    commonSettings
  )

lazy val dayTwoMod = (project in file("day-02-mod"))
  .settings(
    name := "day two (mod)",
    commonSettings
  )

lazy val dayThree = (project in file("day-03"))
  .settings(
    name := "day three",
    commonSettings
  )

lazy val dayFour = (project in file("day-04"))
  .settings(
    name := "day four",
    commonSettings
  )

lazy val dayFive = (project in file("day-05"))
  .settings(
    name := "day five",
    commonSettings
  )

lazy val daySix = (project in file("day-06"))
  .settings(
    name := "day six",
    commonSettings
  )

lazy val daySeven = (project in file("day-07"))
  .settings(
    name := "day seven",
    commonSettings
  )
