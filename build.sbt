name := "learn-cats"

version := "0.1"

val scala3Version = "3.1.2"
val scala2Version = "2.13.7"
val effectVersion = "2.5.3"

lazy val books = project
  .in(file("books"))
  .settings(
    scalaVersion := scala2Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % effectVersion,
      "org.typelevel" %% "cats-effect-laws" % effectVersion
    )
  )

lazy val courses = project
  .in(file("courses"))
  .settings(
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.3.12",
      "org.scalatest" %% "scalatest" % "3.2.12" % "test"
    )
  )
