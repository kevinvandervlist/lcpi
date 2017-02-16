javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint")

// Cats and cats-free version
val catsVersion = "0.9.0"
// scala-parser-combinator version
val sParSecVersion = "1.0.5"


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
    "-unchecked",
    "-Ywarn-unused-import"
  ),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  ),
  // Exclude the Main.scala from coverage reports
  coverageExcludedFiles := ".*Main.*"
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
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % sParSecVersion
    ),
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
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % sParSecVersion,
      "org.typelevel" %% "cats" % catsVersion,
      "org.typelevel" %% "cats-free" % catsVersion,
      "org.jline" % "jline" % "3.1.2"
    ),
    mainClass in assembly := Some("nl.soqua.lcpi.repl.Main")
  ).dependsOn(ast, parser, interpreter % "test->test;compile->compile")
