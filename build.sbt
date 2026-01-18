import com.typesafe.tools.mima.core._

ThisBuild / scalaVersion := "3.7.4"

ThisBuild / tlBaseVersion := "0.1"
ThisBuild / organization := "io.github.arainko"
ThisBuild / organizationName := "arainko"
ThisBuild / startYear := Some(2025)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(tlGitHubDev("arainko", "Aleksander Rainko"))

ThisBuild / mimaBinaryIssueFilters ++= Seq(
  ProblemFilters.exclude[Problem]("chanterelle.internal.*"),
  ProblemFilters.exclude[ReversedMissingMethodProblem]("chanterelle.hidden.Selector.leftElement"),
  ProblemFilters.exclude[ReversedMissingMethodProblem]("chanterelle.hidden.Selector.rightElement"),
  ProblemFilters.exclude[ReversedMissingMethodProblem]("chanterelle.hidden.TupleModifier#package#TupleModifier#Builder.rename")
)

ThisBuild / tlCiReleaseBranches := Seq("main")
ThisBuild / tlCiScalafixCheck := true
ThisBuild / tlCiScalafmtCheck := true
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / githubWorkflowUseSbtThinClient := true
// ThisBuild / semanticdbEnabled := true
// ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / tlVersionIntroduced := Map("3" -> "0.0.0")
ThisBuild / tlCiDependencyGraphJob := false
ThisBuild / githubWorkflowBuild += WorkflowStep.Run(
  name = Some("Check docs"),
  commands = "sbt --client docs/mdoc" :: Nil
)

lazy val root = tlCrossRootProject.aggregate(chanterelle)

lazy val chanterelle =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .withoutSuffixFor(JVMPlatform)
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
        "org.scalameta" %%% "munit" % "1.2.1" % Test
      )
    )
    .jsSettings(
      bspEnabled := false,
      mimaPreviousArtifacts := Set.empty
    )
    .nativeSettings(
      bspEnabled := false,
      mimaPreviousArtifacts := Set.empty
    )

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
