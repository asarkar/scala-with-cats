lazy val scala3Version = "3.3.1"
lazy val scalatestVersion = "3.2.17"
lazy val catsVersion = "2.10.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala-with-cats",
    version := "1.0.0",

    scalaVersion := scala3Version,

    scalafmtPrintDiff := true,

    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % scalatestVersion % Test,
      "org.scalatest" %% "scalatest" % scalatestVersion % Test,
      "org.typelevel" %% "cats-core" % catsVersion
    ),

    scalacOptions ++= Seq(
        "-encoding", "UTF-8",
        "-feature",
        "-Werror",
        "-explain",
        "-deprecation",
        "-unchecked",
        "-Wunused:all",
        "-rewrite",
        "-indent",
        "-source", "future",
      ),
  )
