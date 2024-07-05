import sbt.Keys.resolvers

import scala.collection.immutable.Seq

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.2"

lazy val root = (project in file("."))
  .settings(
    name := "DIALS",
    idePackagePrefix := Some("com.lsc")
  ).aggregate(GenericSimUtilities) enablePlugins Cinnamon dependsOn GenericSimUtilities

Global / excludeLintKeys +=idePackagePrefix

lazy val GenericSimUtilities = (project in file("GenericSimUtilities"))
  .settings(
    name := "GenericSimUtilities",
    libraryDependencies ++= commonDependencies
  )

resolvers += "Akka library repository".at("https://repo.akka.io/maven")
resolvers += ("Apache Snapshots" at "http://repository.apache.org/content/repositories/snapshots").withAllowInsecureProtocol(true)
resolvers += ("Apache repo" at "https://repository.apache.org/").withAllowInsecureProtocol(true)

lazy val akkaVersion = "2.9.3"
lazy val scalaTestVersion = "3.2.17"
lazy val scalaMockitoTestVersion = "3.2.12.0-RC2"
lazy val typeSafeConfigVersion = "1.4.3"
lazy val logbackVersion = "1.4.14"
lazy val apacheCommonMathVersion = "3.6.1"
val apacheCommonIOVersion = "2.11.0"
val scalacticVersion = "3.2.9"
val nscalatimeVersion = "2.30.0"
val catsVersion = "2.7.0"
val catsEffectVersion = "3.4-389-3862cf0"
val typesafeScalaLogging = "3.9.4"
val protobufVersion = "1.0.1"
val catsScalatestEffects = "1.4.0"
val apacheSSHVersion = "2.9.3"
val apacheRngVersion = "1.3"
val guavaVersion = "33.2.1-jre"


fork := true

run / cinnamon := true
test / cinnamon := true
cinnamonLogLevel := "INFO"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
  Cinnamon.library.cinnamonAkka,
  Cinnamon.library.cinnamonAkkaTyped,
  Cinnamon.library.cinnamonPrometheus,
  Cinnamon.library.cinnamonPrometheusHttpServer,
)

lazy val commonDependencies = Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "org.scalatestplus" %% "mockito-4-2" % scalaMockitoTestVersion % Test,
  "com.typesafe" % "config" % typeSafeConfigVersion,
  "ch.qos.logback" % "logback-classic" % logbackVersion,
  "com.github.dwickern" %% "scala-nameof" % "4.0.0" % "provided",
  "com.google.guava" % "guava" % guavaVersion,
  "org.apache.commons" % "commons-math3" % apacheCommonMathVersion,
  "commons-io" % "commons-io" % apacheCommonIOVersion,
  "org.apache.commons" % "commons-math3" % apacheCommonMathVersion,
  "org.apache.commons" % "commons-rng-simple" % apacheRngVersion,
  "org.apache.sshd" % "sshd-core" % apacheSSHVersion
)


scalacOptions ++= Seq(
  "-Werror",
  "-Wimplausible-patterns",
  "-Wnonunit-statement",
  "-Wunused:all",
  "-Ycheck-all-patmat",
  "-Ydebug-pos",
  "-Yexplicit-nulls",
  "-Yrequire-targetName",
  "-Ysafe-init-global",
  "-deprecation",
  "-experimental",
  "-explain",
  "-feature",
  "-language:strictEquality",
  "-new-syntax",
  "-release:17",
  "-source:future",
  "-unchecked",
)
