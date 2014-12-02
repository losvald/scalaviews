name := "scalaviews"

organization := "org"

scalaVersion := "2.10.2"

scalaOrganization := "org.scala-lang.virtualized"

libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % "2.10.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-library" % "2.10.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-reflect" % "2.10.2"

libraryDependencies += "EPFL" %% "lms" % "0.3-SNAPSHOT"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test"

libraryDependencies += "org.scala-lang.virtualized" % "scala-actors" % "2.10.2" % "test"

scalacOptions ++= Seq("-Yvirtualize", "-unchecked", "-deprecation", "-feature")

// We need to run tests serially, since We are using the singleton Driver.
// This also ensures the reported times are accurate for speed tests.
parallelExecution in Test := false

// Comment out the '!' to run speed tests instead of correctness tests
testOptions in Test := Seq(Tests.Filter(s => !
  s.endsWith("Speedtest")))

testOptions in Test += Tests.Argument("-oD")
