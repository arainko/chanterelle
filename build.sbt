import com.typesafe.tools.mima.core._

ThisBuild / scalaVersion := "3.7.1"

ThisBuild / tlBaseVersion := "0.0"
ThisBuild / organization := "io.github.arainko"
ThisBuild / organizationName := "arainko"
ThisBuild / startYear := Some(2025)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(tlGitHubDev("arainko", "Aleksander Rainko"))

ThisBuild / mimaBinaryIssueFilters ++= Seq(
  ProblemFilters.exclude[Problem]("chanterelle.internal.*")
)

ThisBuild / tlCiReleaseBranches := Seq("main")
ThisBuild / tlCiScalafixCheck := true
ThisBuild / tlCiScalafmtCheck := true
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / githubWorkflowUseSbtThinClient := true
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / tlVersionIntroduced := Map("3" -> "0.0.0")
ThisBuild / tlCiDependencyGraphJob := false

lazy val root =
  project
    .in(file("."))
    .enablePlugins(NoPublishPlugin)
    .aggregate(chanterelle)

lazy val chanterelle =
  project
    .in(file("chanterelle"))
    .settings(
      scalacOptions ++= Seq(
        "-preview",
        "-Wunused:all",
        "-WunstableInlineAccessors",
        "-Xcheck-macros",
        "-Wconf:msg=(infix named):s" // TODO: report errors reported without this to dotty (when adding stuff with '+' and the -> syntax into a SortedMap)
      ),
      libraryDependencies ++= Seq(
        "org.scalameta" %% "munit" % "1.1.1" % Test
      )
    )

lazy val docs =
  project
    .in(file("documentation"))
    .enablePlugins(NoPublishPlugin, MdocPlugin)
    .dependsOn(chanterelle)

lazy val generateReadme = taskKey[Unit]("gen readme")

generateReadme := Def.task {
  val docOutput = (docs / mdocOut).value
  IO.copyFile(docOutput / "README.md", file("README.md"))
}.dependsOn((docs / mdoc).toTask("")).value
