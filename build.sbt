// See README.md for license details.

ThisBuild / scalaVersion := "2.13.10"
ThisBuild / version      := "0.1.0"
ThisBuild / organization := "liuyic00"

val chiselVersion = "3.5.6"

lazy val root = (project in file("."))
  .settings(
    name := "cst",
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit"
    )
  )
