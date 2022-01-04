name := "learn-cats"

version := "0.1"

scalaVersion := "2.13.7"

val effectsVersion = "2.5.3"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % effectsVersion,
  "org.typelevel" %% "cats-effect-laws" % effectsVersion
)
