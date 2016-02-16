import com.typesafe.sbt.SbtScalariform

lazy val Benchmark = config("bench") extend Test

/**  This allows running ScalaMeter benchmarks in separate sbt configuration.
  *  It means, that when you want run your benchmarks you should type 
  *  `bench:test` in sbt console.
  */
lazy val basic = Project(
  "basic-with-separate-config",
  file("."),
  settings = Defaults.coreDefaultSettings ++ 
    SbtScalariform.scalariformSettings ++ Seq(
    name := "parsequery",
    organization := "functadelic",
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq(
      "-Xlint",
      "-deprecation",
      "-feature",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-unchecked"
    ),
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.2.5",
      "org.scala-lang" % "scala-compiler" % "2.11.7",
      "com.storm-enroute" %% "scalameter" % "0.7",
      "com.lihaoyi" %% "fastparse" % "0.3.4",
      "org.scalatest" %% "scalatest" % "3.0.0-M15" % "test"
    ),
    resolvers ++= Seq(
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"
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
