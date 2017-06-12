
name := "scala-chess-dsl"

version := "1.0"

scalaVersion := "2.11.8"

lazy val root = (project in file("."))
  .enablePlugins(SbtTwirl)
//  .settings(
//  sourceDirectories in (Compile, TwirlKeys.compileTemplates) += (baseDirectory.value.getParentFile / "src" / "main" / "twirl"))


libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
)