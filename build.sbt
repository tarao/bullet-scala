import sbt._
import Keys._

lazy val bullet = (project in file(".")).
  settings(
    name := "bullet",
    organization := "com.github.tarao",
    version := "0.0.1",
    scalaVersion := "2.11.7",

    // Depenency
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.4" % "test"
    ),

    // Compilation
    scalacOptions ++= Seq(
      "-unchecked",
      "-deprecation",
      "-feature"
    ),

    // Documentation
    scalacOptions in (Compile, doc) ++= Nil :::
      "-groups" ::
      "-sourcepath" ::
      baseDirectory.value.getAbsolutePath ::
      "-doc-source-url" ::
      "https://github.com/tarao/bullet-scala/tree/master€{FILE_PATH}.scala" ::
      Nil,

    // Publishing
    publishMavenStyle := true,
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    pomExtra := (
      <url>https://github.com/tarao/bullet-scala</url>
      <licenses>
        <license>
          <name>MIT License</name>
          <url>http://www.opensource.org/licenses/mit-license.php</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:tarao/bullet-scala.git</url>
        <connection>scm:git:git@github.com:tarao/bullet-scala.git</connection>
      </scm>
      <developers>
        <developer>
          <id>tarao</id>
          <name>INA Lintaro</name>
          <url>https://github.com/tarao/</url>
        </developer>
      </developers>)
  )
