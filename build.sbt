name := "functadelic"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "0.3.4",
  "com.storm-enroute" %% "scalameter" % "0.7",
  "org.scalatest" %% "scalatest" % "3.0.0-M15" % "test"
)

scalacOptions ++= Seq(
  //"-optimize",
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked"
)

defaultScalariformSettings

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
logBuffered := false

parallelExecution in Test := false
