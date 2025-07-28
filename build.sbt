ThisBuild / scalaVersion := "3.7.1"

lazy val root = project.in(file(".")).aggregate(chanterelle)

lazy val chanterelle =
  project
    .in(file("chanterelle"))
    .settings(
      scalaVersion := "3.7.1",
      scalacOptions ++= Seq(
        "-preview",
        "-Wunused:all",
        "-WunstableInlineAccessors",
        "-Xcheck-macros",
        "-Wconf:msg=(infix named):s" // TODO: report errors reported without this to dotty (when addint stuff with '+' and the -> syntax into a SortedMap)
      ),
      libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test
    )
