scalaVersion := "2.13.3"


name := "homework"
version := "1.0"

val catsVersion = "2.2.0"
val scalaTestVersion = "3.1.0.0-RC2"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.typelevel" %% "cats-core" % "2.3.1",
  "org.scalatest" %% "scalatest" % "3.2.3" % Test

)

