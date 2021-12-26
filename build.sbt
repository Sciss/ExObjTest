lazy val baseName         = "ExObjTest"
lazy val baseNameL        = baseName.toLowerCase
lazy val projectVersion   = "0.1.0-SNAPSHOT"

lazy val deps = new {
  val lucre = "4.5.3"
}

// sonatype plugin requires that these are in global
ThisBuild / version       := projectVersion
ThisBuild / organization  := "de.sciss"
ThisBuild / versionScheme := Some("pvp")

lazy val commonSettings = Seq(
  description         := "Experiment to add full Lucre Obj type based on Lucre Ex",
  homepage            := Some(url(s"https://github.com/Sciss/$baseName")),
  scalaVersion        := "2.13.7",
  scalacOptions      ++= Seq(
    "-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Ywarn-unused:params,-implicits"
  ),
  scalacOptions ++= {
    // if (isDotty.value) Nil else 
    Seq("-Xlint", "-Xsource:2.13")
  },
  Compile / compile / scalacOptions ++= {
    val jdkGt8 = scala.util.Properties.isJavaAtLeast("9")
    if (jdkGt8) Seq("-release", "8") else Nil
  },
  Test / testOptions += Tests.Argument("-oDF"),   // ScalaTest: durations and full stack traces
  Test / parallelExecution := false,
  licenses := Seq(agpl),
) ++ publishSettings

lazy val agpl = "AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")

lazy val root = project.withId(baseNameL).in(file("."))
  .settings(commonSettings)
  .settings(
    name := baseName,
    libraryDependencies ++= Seq(
      "de.sciss" %%% "serial" % deps.lucre,
    ),
  )

