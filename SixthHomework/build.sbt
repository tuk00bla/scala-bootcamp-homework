import sbt.project


name := "SixthHomework"

version := "0.2"

scalaVersion := "2.13.4"


enablePlugins(BulkySourcesPlugin)

lazy val bulkySourcesPlugin = (project in file("BulkySourcePlugin"))
  .enablePlugins(SbtPlugin)
  .settings(
    ThisBuild / version := "0.1",
    ThisBuild / organization := "com.example",
    name := "BulkySourcesPlugin",
    scalaVersion := "2.12.1"
  )