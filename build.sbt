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
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq(
        s"-P:scalajs:mapSourceURI:$localSourcesPath->$remoteSourcesPath",
        "-P:scalajs:genStaticForwardersForNonTopLevelObjects"
      )
      case _ => Seq(
        s"-scalajs-mapSourceURI:$localSourcesPath->$remoteSourcesPath",
        "-scalajs-genStaticForwardersForNonTopLevelObjects"
      )
    }
  }
)

lazy val nativeSettings = Seq(
  scalacOptions ++= Seq("-P:scalanative:genStaticForwardersForNonTopLevelObjects"),
)

lazy val root =
  project
    .in(file("."))
    .enablePlugins(NoPublishPlugin)
    .aggregate(
      chanterelle.jvm,
      chanterelle.js,
      chanterelle.native
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
    .jsSettings(jsSettings)
    .nativeSettings(nativeSettings)

lazy val docs =
  project
    .in(file("documentation"))
    .enablePlugins(NoPublishPlugin, MdocPlugin)
    .dependsOn(chanterelle.jvm)

lazy val generateReadme = taskKey[Unit]("gen readme")

generateReadme := Def.task {
  val docOutput = (docs / mdocOut).value
  IO.copyFile(docOutput / "README.md", file("README.md"))
}.dependsOn((docs / mdoc).toTask("")).value
