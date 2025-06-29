scalaVersion := "3.7.1"

scalacOptions ++= Seq(
  "-Wunused:all",
  "-WunstableInlineAccessors",
  "-Xcheck-macros",
  "-Wconf:msg=(infix named):s" //TODO: report errors reported without this to dotty (when addint stuff with '+' and the -> syntax into a SortedMap)
)
