name := "TenthHomework"

version := "0.2"

scalaVersion := "2.13.4"
val scalaTestVersion = "3.1.0.0-RC2"
val catsVersion = "2.2.0"
val catsTaglessVersion = "0.11"
val catsEffectVersion = "2.2.0"
val circeVersion = "0.13.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion % Test
libraryDependencies += "org.scalatestplus" %% "selenium-2-45" % scalaTestVersion % Test
libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion
libraryDependencies += "org.typelevel" %% "cats-effect" % catsEffectVersion
libraryDependencies += "io.circe" %% "circe-core" % circeVersion
libraryDependencies += "io.circe" %% "circe-generic" % circeVersion
libraryDependencies += "io.circe" %% "circe-generic-extras" % circeVersion
libraryDependencies += "io.circe" %% "circe-optics" % circeVersion
libraryDependencies += "io.circe" %% "circe-parser" % circeVersion
libraryDependencies += "org.scalaj" %% "scalaj-http" % "2.4.2" % Test