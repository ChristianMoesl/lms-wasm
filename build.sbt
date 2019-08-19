name := "lms-wasm"

version := "0.0.1"

scalaVersion := "2.12.8"

val paradiseVersion = "2.1.0"

//crossScalaVersions := Seq("2.12.1")

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
libraryDependencies += "org.scala-lang.virtualized" %% "lms-clean" % "0.0.1-SNAPSHOT"

libraryDependencies += ("org.scala-lang" % "scala-reflect" % scalaVersion.value)

libraryDependencies += ("org.scala-lang" % "scala-compiler" % scalaVersion.value % "compile")

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)

addCompilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.12.0" % "1.0.3")

libraryDependencies += "org.scala-lang.plugins" % "scala-continuations-library_2.12" % "1.0.3"

scalacOptions += "-P:continuations:enable"

// tests are not thread safe
parallelExecution in Test := false

// do not include repl scripts in assembly
sourcesInBase := false
