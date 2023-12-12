val tapirVersion = "1.8.1"
val MunitCatsEffectVersion = "1.0.7"

val MUnitFramework = new TestFramework("munit.Framework")

Compile / run / fork := true

enablePlugins(CalibanPlugin)

lazy val rootProject = (project in file(".")).settings(
  Seq(
    name := "grvx-dbin-api",
    version := "0.1.0-SNAPSHOT",
    organization := "fun.gravax",
    scalaVersion := "3.3.1",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % tapirVersion,
      "org.http4s" %% "http4s-ember-server" % "0.23.24",
      "com.softwaremill.sttp.tapir" %% "tapir-prometheus-metrics" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-bundle" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirVersion,

      "com.softwaremill.sttp.tapir" %% "tapir-aws-lambda" % tapirVersion, // Docs say "1.9.0"
      "com.softwaremill.sttp.tapir" %% "tapir-aws-sam" % tapirVersion, // "1.9.0"

      "com.github.ghostdogpr" %% "caliban-client" % "2.4.3",
      "com.softwaremill.sttp.client3"         %% "zio"                   % "3.9.1",

      "ch.qos.logback" % "logback-classic" % "1.4.11",
      "com.softwaremill.sttp.tapir" %% "tapir-sttp-stub-server" % tapirVersion % Test,
      "com.softwaremill.sttp.client3" %% "circe" % "3.9.1" % Test,
      // "org.scalatest" %% "scalatest" % "3.2.17" % Test,

      "org.typelevel" %% "munit-cats-effect-3" % MunitCatsEffectVersion % Test
    )


  )
)
