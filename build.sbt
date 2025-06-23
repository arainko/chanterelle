scalaVersion := "3.7.1"

scalacOptions ++= Seq("-Wunused:all", "-WunstableInlineAccessors", "-Xcheck-macros")
libraryDependencies += "dev.zio" %% "zio" % "2.1.19"
