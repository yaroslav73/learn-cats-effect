name := "learn-cats"

version := "0.1"

val scala3Version    = "3.2.2"
val scala2Version    = "2.13.11"
val ce2Version       = "2.5.3"
val ce3Version       = "3.4.10"
val scalaTestVersion = "3.2.12"

lazy val books = project
  .in(file("books"))
  .settings(
    scalaVersion := scala2Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect"      % ce2Version,
      "org.typelevel" %% "cats-effect-laws" % ce2Version
    )
  )

lazy val courses = project
  .in(file("courses"))
  .settings(
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % ce3Version,
      "org.scalatest" %% "scalatest"   % scalaTestVersion % "test"
    )
  )

lazy val playground = project
  .in(file("playground"))
  .settings(
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % ce3Version,
      "org.scalatest" %% "scalatest"   % scalaTestVersion % "test"
    )
  )
