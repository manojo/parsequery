import sbt._
import Keys._

object ParsequeryBuild extends Build {
  scalaVersion := "2.11.8"

  def commonSettings = Seq(
    organization := "com.github.manojo",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.8",
    scalacOptions += "-deprecation",
    scalacOptions += "-unchecked",
    scalacOptions += "-feature",
    scalacOptions += "-language:higherKinds",
    scalacOptions += "-language:experimental.macros",
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
  ) ++ publishSettings

  lazy val Benchmark = config("bench") extend Test

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = commonSettings ++ Seq(
      run <<= run in Compile in parsequery)
  ) aggregate(parsequery, examples)

  lazy val parsequery: Project = Project(
    id = "parsequery",
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
    ) ++ publishableSettings
  ) configs(
    Benchmark
  ) settings(
    inConfig(Benchmark)(Defaults.testSettings): _*
  )

  lazy val examples: Project = Project(
    id = "parsequery_examples",
    base = file("core"),
    dependencies = Seq(parsequery),
    settings = commonSettings ++ Seq(
      // include the macro classes and resources in the main jar
      mappings in (Compile, packageBin) ++= mappings.in(parsequery, Compile, packageBin).value,
      // include the macro sources in the main source jar
      mappings in (Compile, packageSrc) ++= mappings.in(parsequery, Compile, packageSrc).value
    )
  )

  /**
   * We are able to publish this thing!
   */
  lazy val publishSettings = Seq(
    publishMavenStyle := true,
    publishOnlyWhenOnMaster := publishOnlyWhenOnMasterImpl.value,
    publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    pomIncludeRepository := { x => false },
    publishArtifact in Compile := false,
    publishArtifact in Test := false,
    pomExtra := (
      <url>https://github.com/manojo/parsequery</url>
      <inceptionYear>2016</inceptionYear>
      <licenses>
        <license>
          <name>MIT</name>
          <url>https://github.com/manojo/parsequery/blob/master/LICENSE</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git:github.com/manojo/parsequery.git</url>
        <connection>scm:git:git://github.com/manojo/parsequery.git</connection>
      </scm>
      <issueManagement>
        <system>GitHub</system>
        <url>https://github.com/manojo/parsequery/issues</url>
      </issueManagement>
    ),
    publishArtifact in (Compile, packageDoc) := false
  )

  lazy val publishOnlyWhenOnMaster = taskKey[Unit](
    "publish task for Travis (don't publish when building pull requests, only publish" +
    "when the build is triggered by merge into master)")

  def publishOnlyWhenOnMasterImpl = Def.taskDyn {
    import scala.util.Try
    val travis   = Try(sys.env("TRAVIS")).getOrElse("false") == "true"
    val pr       = Try(sys.env("TRAVIS_PULL_REQUEST")).getOrElse("false") != "false"
    val branch   = Try(sys.env("TRAVIS_BRANCH")).getOrElse("??")
    val snapshot = version.value.trim.endsWith("SNAPSHOT")
    (travis, pr, branch, snapshot) match {
      case (true, false, "master", true) => publish
      case _                             => Def.task ()
    }
  }

  lazy val publishableSettings = Seq(
    publishArtifact in Compile := true,
    publishArtifact in Test := false,
    credentials ++= {
      val mavenSettingsFile = System.getenv("MAVEN_SETTINGS_FILE")
      if (mavenSettingsFile != null) {
        println("Loading Sonatype credentials from " + mavenSettingsFile)
        try {
          import scala.xml._
          val settings = XML.loadFile(mavenSettingsFile)
          def readServerConfig(key: String) = (settings \\ "settings" \\ "servers" \\ "server" \\ key).head.text
          Some(Credentials(
            "Sonatype Nexus Repository Manager",
            "oss.sonatype.org",
            readServerConfig("username"),
            readServerConfig("password")
          ))
        } catch {
          case ex: Exception =>
            println("Failed to load Maven settings from " + mavenSettingsFile + ": " + ex)
            None
        }
      } else {
        for {
          realm <- sys.env.get("SCALAMETA_MAVEN_REALM")
          domain <- sys.env.get("SCALAMETA_MAVEN_DOMAIN")
          user <- sys.env.get("SCALAMETA_MAVEN_USER")
          password <- sys.env.get("SCALAMETA_MAVEN_PASSWORD")
        } yield {
          println("Loading Sonatype credentials from environment variables")
          Credentials(realm, domain, user, password)
        }
      }
    }.toList
  )
}
