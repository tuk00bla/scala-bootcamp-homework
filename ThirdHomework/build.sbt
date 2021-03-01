name := "ThirdHomework"

version := "0.1"

scalaVersion := "2.13.4"

val catsVersion = "2.2.0"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.3" % Test,
  "org.scalatestplus" %% "scalacheck-1-15" % "3.2.3.0" % Test
)
libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion
