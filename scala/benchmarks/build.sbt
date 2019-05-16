// The simplest possible sbt build file is just one line:

scalaVersion := "2.12.7"
// That is, to create a valid sbt build, all you've got to do is define the
// version of Scala you'd like your project to use.

// ============================================================================

name := "benchmarks"

// Want to use a published library in your project?
// You can define other libraries as dependencies in your build like this:
libraryDependencies += "org.typelevel" %% "cats-core" % "1.4.0"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.17"
libraryDependencies += "io.argonaut" %% "argonaut" % "6.2.2"

testFrameworks += new TestFramework(
  "org.scalameter.ScalaMeterFramework")

logBuffered := false

parallelExecution in Test := false

scalacOptions ++= Seq(
  "-optimize",
  "-opt:_"
)