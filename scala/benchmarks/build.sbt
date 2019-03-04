// The simplest possible sbt build file is just one line:

scalaVersion := "2.12.7"
// That is, to create a valid sbt build, all you've got to do is define the
// version of Scala you'd like your project to use.

// ============================================================================

name := "benchmarks"

// Want to use a published library in your project?
// You can define other libraries as dependencies in your build like this:
libraryDependencies += "org.typelevel" %% "cats-core" % "1.4.0"