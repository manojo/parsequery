import sbt._
import Keys._
import com.typesafe.sbt.SbtScalariform

trait BuildAndSettings extends Build {
  val buildSettings = super.settings ++ Seq(
    organization := "org.scalamacros",
    version := "1.0.0",
    scalaVersion := "2.11.7",
    crossScalaVersions := Seq("2.10.2", "2.10.3", "2.10.4", "2.10.5", "2.11.0", "2.11.1", "2.11.2", "2.11.3", "2.11.4", "2.11.5", "2.11.6", "2.11.7"),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    scalacOptions ++= Seq(
      //"-optimize",
      "-deprecation",
      "-feature",
      "-language:higherKinds",
      "-language:implicitConversions"
    )
  )
}

object ParsequeryBuild extends BuildAndSettings {
  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(
      run <<= run in Compile in core)
  ) aggregate(macros, core)

  lazy val macros: Project = Project(
    "macros",
    file("macros"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies := {
        CrossVersion.partialVersion(scalaVersion.value) match {
          // if Scala 2.11+ is used, quasiquotes are available in the standard distribution
          case Some((2, scalaMajor)) if scalaMajor >= 11 =>
            libraryDependencies.value
          // in Scala 2.10, quasiquotes are provided by macro paradise
          case Some((2, 10)) =>
            libraryDependencies.value ++ Seq(
              compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full),
              "org.scalamacros" %% "quasiquotes" % "2.1.0-M5" cross CrossVersion.binary)
        }
      }
    )
  )

  lazy val core: Project = Project(
    "core",
    file("core"),
    settings = buildSettings ++ SbtScalariform.scalariformSettings ++ Seq(
      testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
      libraryDependencies := {
        libraryDependencies.value ++ Seq(
          "com.lihaoyi" %% "fastparse" % "0.3.4",
          "com.storm-enroute" %% "scalameter" % "0.7"
        )
      }
    )
  ) dependsOn(macros)
}

/*name := "functadelic"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "0.3.4",
  "com.storm-enroute" %% "scalameter" % "0.7"
)

scalacOptions ++= Seq(
  //"-optimize",
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions"
)

defaultScalariformSettings

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
logBuffered := false

parallelExecution in Test := false */
