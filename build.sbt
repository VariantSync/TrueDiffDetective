name := "TrueDiffDetective"

ThisBuild / organization := "org.variantsync"
// The version is duplicated in `default.nix`.
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.1"

ThisBuild / useCoursier := false
ThisBuild / resolvers += Resolver.mavenLocal

lazy val TrueDiffDetective = (project in file("."))
  .settings(
    name := "TrueDiffDetective",
    scalacOptions ++= Seq(
      "-Ymacro-annotations"
    ),
    libraryDependencies ++= Seq(
      "de.uni-mainz.informatik.pl" % "truechange_2.13" % "0.2.0-SNAPSHOT",
      "de.uni-mainz.informatik.pl" % "truediff_2.13" % "0.2.0-SNAPSHOT",
      "org.variantsync" % "diffdetective" % "2.2.0"
    )
  )