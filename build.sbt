import com.typesafe.tools.mima.core._

ThisBuild / scalaVersion := "3.7.2"

ThisBuild / tlBaseVersion := "0.1"
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
ThisBuild / githubWorkflowBuild += WorkflowStep.Run(
  name = Some("Check docs"),
  commands = "sbt --client docs/mdoc" :: Nil
)

lazy val jsSettings = Seq(
  scalacOptions ++= {
    val localSourcesPath = (LocalRootProject / baseDirectory).value.toURI
    val remoteSourcesPath = s"https://raw.githubusercontent.com/arainko/chanterelle/${git.gitHeadCommit.value.get}/"
    Seq(
      s"-scalajs-mapSourceURI:$localSourcesPath->$remoteSourcesPath",
      "-scalajs-genStaticForwardersForNonTopLevelObjects"
    )
  },
  bspEnabled := false,
  mimaPreviousArtifacts := Set()
)

lazy val nativeSettings = Seq(
  scalacOptions ++= Seq("-P:scalanative:genStaticForwardersForNonTopLevelObjects"),
  bspEnabled := false,
  mimaPreviousArtifacts := Set()
)

lazy val root =
  project
    .in(file("."))
    .enablePlugins(NoPublishPlugin)
    .aggregate(
      chanterelleJVM,
      chanterelleJS,
      chanterelleNative
    )

lazy val chanterelle =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("chanterelle"))
    .settings(
      scalacOptions ++= Seq(
        "-preview",
        "-Wunused:all",
        "-WunstableInlineAccessors",
        "-Xcheck-macros",
        "-Wconf:msg=(infix named):s,msg=(@nowarn annotation does not):s" // TODO: report errors reported without this to dotty (when adding stuff with '+' and the -> syntax into a SortedMap)
      ),
      libraryDependencies ++= Seq(
        "org.scalameta" %%% "munit" % "1.1.1" % Test
      )
    )

lazy val chanterelleJVM =
  chanterelle.jvm

lazy val chanterelleJS =
  chanterelle.js
    .settings(jsSettings)

lazy val chanterelleNative =
  chanterelle.native
    .settings(nativeSettings)

lazy val docs =
  project
    .in(file("documentation"))
    .enablePlugins(NoPublishPlugin, MdocPlugin)
    .dependsOn(chanterelleJVM)

lazy val generateReadme = taskKey[Unit]("gen readme")

generateReadme := Def.task {
  val docOutput = (docs / mdocOut).value
  IO.copyFile(docOutput / "README.md", file("README.md"))
}.dependsOn((docs / mdoc).toTask("")).value
