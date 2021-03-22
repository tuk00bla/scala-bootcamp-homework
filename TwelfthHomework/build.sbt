name := "TwelfthHomework"

version := "0.1"

scalaVersion := "2.13.5"

scalaVersion := "2.13.3"

val catsVersion = "2.2.0"
val catsEffectVersion = "2.2.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion
)
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full)