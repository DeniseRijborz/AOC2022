val TestDependencies = Seq(
  "org.scalatest"  %% "scalatest"  % "3.2.14" % "test"
)

lazy val aoc = project
  .in(file("aoc"))
  .settings( name           := "aoc"
    , version              := "0.1.0"
    , scalaVersion         := "3.2.0"
    , libraryDependencies ++= TestDependencies
  )
