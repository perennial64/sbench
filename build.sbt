import org.scalajs.linker.interface.ModuleKind
import scala.scalanative.build.LTO
import scala.scalanative.build.Mode

val scala212 = "2.12.20"
val scala213 = "2.13.17"
val scala3 = "3.3.7"

val allScalaVersions = Seq(scala212, scala213, scala3)

ThisBuild / organization := "xyz.perennial"
ThisBuild / organizationName := "perennial"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := scala3
ThisBuild / versionScheme := Some("early-semver")

lazy val commonSettings = Seq(
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-encoding", "utf-8"),
  scalacOptions ++=
    (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq("-Xlint")
      case _ => Seq("-source:3.3", "-Wunused:all")
    }),
  libraryDependencies += "org.scalameta" %%% "munit" % "1.2.0" % Test,
  testFrameworks += new TestFramework("munit.Framework"),
)

lazy val sbench = projectMatrix
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "sbench",
    description := "sbench",
    libraryDependencies += "com.lihaoyi" %%% "fansi" % "0.5.1",
  )
  .jvmPlatform(scalaVersions = allScalaVersions)
  .jsPlatform(
    scalaVersions = allScalaVersions,
    settings = Seq(scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))),
  )
  .nativePlatform(scalaVersions = allScalaVersions)

lazy val examples = projectMatrix
  .in(file("examples"))
  .dependsOn(sbench)
  .settings(commonSettings)
  .settings(
    name := "sbench-examples",
    description := "sbench examples",
    publish / skip := true,
  )
  .jvmPlatform(scalaVersions = Seq(scala3))
  .jsPlatform(
    scalaVersions = Seq(scala3),
    settings = Seq(
      scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
      scalaJSUseMainModuleInitializer := true,
    ),
  )
  .nativePlatform(
    scalaVersions = Seq(scala3),
    settings = Seq(nativeConfig ~= (_.withMode(Mode.releaseFast).withLTO(LTO.thin))),
  )

lazy val root = project
  .in(file("."))
  .aggregate(sbench.projectRefs ++ examples.projectRefs: _*)
  .settings(
    name := "sbench-root",
    publish / skip := true,
  )

addCommandAlias("fmt", List("scalafmtAll", "scalafmtSbt").mkString(";", " ;", ""))

addCommandAlias(
  "fmtCheck",
  List("scalafmtCheckAll", "scalafmtSbtCheck").mkString(";", " ;", ""),
)

// Use this command before submitting a PR
addCommandAlias(
  "lint",
  List(
    "clean",
    "Test/compile",
    "scalafmtAll",
    "scalafmtSbt",
  ).mkString(";", " ;", ""),
)
