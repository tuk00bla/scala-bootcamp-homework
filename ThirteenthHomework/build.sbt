name := "ThirteenthHomework"

version := "0.1"

scalaVersion := "2.13.5"

val catsVersion = "2.2.0"
val catsEffectVersion = "2.2.0"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
  "org.scalatest" %% "scalatest" % "3.2.3" % Test,
  "org.scalaj" %% "scalaj-http" % "2.4.2" % Test,

)