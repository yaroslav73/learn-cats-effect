name := "learn-cats"

version := "0.1"

val scala3Version    = "3.5.2"
val scala2Version    = "2.13.11"
val ce2Version       = "2.5.3"
val ce3Version       = "3.4.10"
val ceLaws2Version   = "2.5.5"
val ceLaws3Version   = "3.5.2"
val scalaTestVersion = "3.2.12"
val ce2Weaver        = "0.6.15"

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

lazy val playgroundCE2 = project
  .in(file("playground-ce-2"))
  .settings(
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel"       %% "cats-effect"      % ce2Version,
      "org.typelevel"       %% "cats-effect-laws" % ceLaws2Version   % Test,
      "org.scalatest"       %% "scalatest"        % scalaTestVersion % Test,
      "com.disneystreaming" %% "weaver-cats"      % ce2Weaver        % Test
    )
  )

lazy val playgroundCE3 = project
  .in(file("playground-ce-3"))
  .settings(
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect"      % ce3Version,
      "org.typelevel" %% "cats-effect-laws" % ceLaws3Version   % Test,
      "org.scalatest" %% "scalatest"        % scalaTestVersion % Test
    )
  )
