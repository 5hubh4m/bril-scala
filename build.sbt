ThisBuild / scalaVersion     := "2.13.7"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "edu.cornell"
ThisBuild / organizationName := "Cornell University"
ThisBuild / scalacOptions ++= Seq("-unchecked", "-deprecation")

lazy val root = (project in file("."))
  .settings(
    name := "bril-scala",
    libraryDependencies += "io.spray" %%  "spray-json" % "1.3.6"
  )
