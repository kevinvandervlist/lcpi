javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint")

lazy val commonSettings = Seq(
  organization := "nl.soqua",
  name := "lcpi",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.12.1",
  test in assembly := {}, // don't run tests on assembly
  scalacOptions ++= Seq(
    "-feature",
    "-Xfatal-warnings",
    "-deprecation",
    "-unchecked"
  ),
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
    "org.scalatest" %% "scalatest" % "3.0.0" % "test"
  ),
  // Exclude the *repl* package from coverage reports
  coverageExcludedPackages := "nl.soqua.lcpi.repl.*"
)

lazy val ast = (project in file("ast")).
  settings(commonSettings: _*).
  settings(
    name := "ast",
    assemblyJarName in assembly := "ast.jar"
  )

lazy val parser = (project in file("parser")).
  settings(commonSettings: _*).
  settings(
    name := "parser",
    assemblyJarName in assembly := "parser.jar"
  ).dependsOn(ast)

lazy val interpreter = (project in file("interpreter")).
  settings(commonSettings: _*).
  settings(
    name := "interpreter",
    assemblyJarName in assembly := "interpreter.jar"
  ).dependsOn(ast, parser)

lazy val repl = (project in file("repl")).
  settings(commonSettings: _*).
  settings(
    name := "repl",
    assemblyJarName in assembly := "repl.jar",
    mainClass in assembly := Some("nl.soqua.lcpi.repl.Main")
  ).dependsOn(ast, parser, interpreter)
