import sbt._
import Keys._

object ParsequeryBuild extends Build {
  scalaVersion := "2.11.8"

  def commonSettings = Seq(
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.8",
    scalacOptions += "-deprecation",
    scalacOptions += "-unchecked",
    scalacOptions += "-feature",
    scalacOptions += "-language:higherKinds",
    //scalacOptions += "-Xprint:typer",
    //scalacOptions += "-uniqid",
    //scalacOptions += "-Yshow-syms",
    libraryDependencies ++=  Seq(
      "org.scala-lang" % "scala-compiler"  % scalaVersion.value % "provided",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ),
    resolvers ++= Seq(
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"
    )
  )

  lazy val Benchmark = config("bench") extend Test

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = commonSettings ++ Seq(
      run <<= run in Compile in core)
  ) aggregate(macros, core)

  lazy val macros: Project = Project(
    id = "macros",
    base = file("macros"),
    settings = commonSettings ++ Seq(
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "3.0.0-M15" % "test",
        "org.scalacheck" %% "scalacheck" % "1.11.5" % "test",
        "com.storm-enroute" %% "scalameter" % "0.7",
        "com.chuusai" %% "shapeless" % "2.2.5",
        "com.lihaoyi" %% "pprint" % "0.3.8",
        "com.lihaoyi" %% "fastparse" % "0.3.7"
      ),

      testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
      parallelExecution in Benchmark := false,
      parallelExecution in Test := false,
      logBuffered := false
    )
  ) configs(
    Benchmark
  ) settings(
    inConfig(Benchmark)(Defaults.testSettings): _*
  )

  lazy val core: Project = Project(
    id = "core",
    base = file("core"),
    dependencies = Seq(macros),
    settings = commonSettings ++ Seq(
      // include the macro classes and resources in the main jar
      mappings in (Compile, packageBin) ++= mappings.in(macros, Compile, packageBin).value,
      // include the macro sources in the main source jar
      mappings in (Compile, packageSrc) ++= mappings.in(macros, Compile, packageSrc).value
    )
  )
}
